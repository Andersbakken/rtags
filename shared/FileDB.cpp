#include "FileDB.h"
#include "Mmap.h"
#include "MmapDevice.h"
#include <QHash>
#include <QMap>
#include <QIODevice>
#include <QDataStream>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

class FileIndex
{
public:
    enum FindType { Exact, LowerBound, UpperBound };

    FileIndex();
    FileIndex(Mmap* idx, Mmap* db);

    void init(Mmap* idx, Mmap* db);
    void clear();

    void read();
    void write();

    int add(const QByteArray& key, int offset);
    int remove(const QByteArray& key);
    bool find(FindType type, const QByteArray& key, int* offset, int* size = 0);

    void maybeSort();

    struct Entry
    {
        int offset;
        int size;
        Mmap* mmap;

        QByteArray key() const;
    };

    QMap<QByteArray, Entry> sorted;
    QHash<QByteArray, Entry> unsorted;
    bool dirty;
    Mmap* dbm;
    Mmap* idxm;
};

QByteArray FileIndex::Entry::key() const
{
    Q_ASSERT(offset > 0);
    mmap->seek(offset);
    return mmap->get<QByteArray>();
}

FileIndex::FileIndex()
    : dirty(false), dbm(0), idxm(0)
{
}

FileIndex::FileIndex(Mmap* idx, Mmap* db)
{
    init(idx, db);
}

void FileIndex::init(Mmap* idx, Mmap* db)
{
    dirty = false;
    idxm = idx;
    dbm = db;
    read();
}

void FileIndex::clear()
{
    if (idxm == 0)
        return;

    idxm = 0;
    dbm = 0;
    sorted.clear();
    unsorted.clear();
}

void FileIndex::read()
{
    sorted.clear();
    unsorted.clear();

    idxm->seek(0);
    MmapDevice dev(idxm);
    QDataStream stream(&dev);
    stream >> sorted;

    QMap<QByteArray, Entry>::iterator i = sorted.begin();
    const QMap<QByteArray, Entry>::const_iterator end = sorted.end();
    while (i != end) {
        i.value().mmap = dbm;
        unsorted[i.key()] = i.value();
        ++i;
    }
}

void FileIndex::maybeSort()
{
    if (!dirty)
        return;
    dirty = false;
    QHash<QByteArray, Entry>::const_iterator i = unsorted.begin();
    const QHash<QByteArray, Entry>::const_iterator end = unsorted.end();
    while (i != end) {
        sorted[i.key()] = i.value();
        ++i;
    }
}

void FileIndex::write()
{
    maybeSort();

    idxm->trunc(0);
    MmapDevice dev(idxm);
    QDataStream stream(&dev);
    stream << sorted;
}

int FileIndex::add(const QByteArray &key, int offset)
{
    QHash<QByteArray, Entry>::iterator i = unsorted.find(key);
    if (i != unsorted.end()) {
        Entry& e = i.value();
        const int prev = e.offset;
        e.offset = offset;
        dirty = true;
        return prev;
    }

    Entry e;
    e.size = key.size();
    e.mmap = dbm;
    e.offset = offset;
    unsorted[key] = e;
    dirty = true;
    return -1;
}

bool FileIndex::find(FindType type, const QByteArray &key, int *offset, int *size)
{
    if (type == Exact) {
        QHash<QByteArray, Entry>::const_iterator i = unsorted.find(key);
        if (i == unsorted.constEnd())
            return false;

        const Entry& e = i.value();
        *offset = e.offset;
        if (size)
            *size = e.size;
        return true;
    }

    maybeSort();

    QMap<QByteArray, Entry>::const_iterator i;
    switch (type) {
    case LowerBound:
        i = sorted.lowerBound(key);
        break;
    case UpperBound:
        i = sorted.upperBound(key);
        break;
    default:
        Q_ASSERT(0 || "FileIndex::find reached default");
        return false;
    }

    if (i == sorted.constEnd())
        return false;

    const Entry& e = i.value();
    *offset = e.offset;
    if (size)
        *size = e.size;
    return true;
}

int FileIndex::remove(const QByteArray &key)
{
    QHash<QByteArray, Entry>::iterator i = unsorted.find(key);
    if (i == unsorted.constEnd())
        return -1;
    const int off = i.value().offset;
    unsorted.erase(i);
    dirty = true;
    return off;
}

class FileConnection : public Connection
{
public:
    FileConnection(FileDB::ConnectionType t, FileDB::Mode mode, const Path& p);
    ~FileConnection();

    virtual QByteArray readData(const QByteArray &key) const;
    virtual void writeData(const QByteArray &key, const QByteArray &value);

private:
    FileDB::ConnectionType type;
    FileDB::Mode mode;
    mutable Mmap dbm, idxm;
    mutable FileIndex idx;
};

FileConnection::FileConnection(FileDB::ConnectionType t, FileDB::Mode m, const Path& p)
    : type(t), mode(m)
{
    char fn = char('a' + t);
    dbm.load(p + "/" + fn + ".db");
    idxm.load(p + "/" + fn + ".idx");

    idx.init(&idxm, &dbm);
}

FileConnection::~FileConnection()
{
    if (mode != FileDB::ReadOnly)
        idx.write();
}

QByteArray FileConnection::readData(const QByteArray &key) const
{
    int offset, size;
    if (idx.find(FileIndex::Exact, key, &offset, &size)) {
        if (size == 0)
            return QByteArray();
        dbm.seek(offset + size + sizeof(int));
        QByteArray v = dbm.get<QByteArray>();
        return v;
    }
    return QByteArray();
}

void FileConnection::writeData(const QByteArray &key, const QByteArray &value)
{
    Q_ASSERT(!key.isEmpty());

    if (value.isEmpty()) {
        const int at = idx.remove(key);
        if (at == -1)
            return;
        dbm.seek(at);
        dbm.set(QByteArray(1, '\0'));
        return;
    }

    const unsigned int pos = dbm.size();
    dbm.seek(pos);
    const unsigned int off = dbm.offset();
    dbm.set(key);
    dbm.set(value);
    const int prev = idx.add(key, off);
    if (prev == -1)
        return;
    dbm.seek(prev);
    dbm.set(QByteArray(1, '\0'));
}

class FileIterator : public Database::iterator
{
public:
    FileIterator(FileDB::ConnectionType t, FileDB::Mode m, const Path& p);

    virtual QByteArray value() const;
    virtual QByteArray key() const;
    virtual bool seek(const QByteArray &key);
    virtual bool next();
    virtual bool isValid() const;
private:
    void open(FileDB::ConnectionType t);

private:
    Path path;
    FileIndex idx;
    Mmap dbm, idxm;
    int db;
    FileDB::Mode mode;

    QByteArray k, v;
};

FileIterator::FileIterator(FileDB::ConnectionType t, FileDB::Mode m, const Path& p)
    : Database::iterator(t), path(p), mode(m)
{
    open(t);
}

void FileIterator::open(FileDB::ConnectionType t)
{
    dbm.clear(Mmap::Sync);
    idx.clear();

    char fn = char('a' + t);
    idxm.load(path + "/" + fn + ".idx");
    dbm.load(path + "/" + fn + ".db");

    idx.init(&idxm, &dbm);

    next();
}

QByteArray FileIterator::value() const
{
    return v;
}

QByteArray FileIterator::key() const
{
    return k;
}

bool FileIterator::seek(const QByteArray &key)
{
    int offset;
    if (idx.find(FileIndex::LowerBound, key, &offset)) {
        dbm.seek(offset);
        next();
        return true;
    }
    return false;
}

bool FileIterator::next()
{
    bool ok;
    forever {
        k = dbm.get<QByteArray>(&ok);
        if (!ok) {
            k.clear();
            v.clear();
            return false;
        }
        v = dbm.get<QByteArray>();

        Q_ASSERT(!k.isEmpty());
        if (k.at(0) != '\0')
            break;
    }
    return true;
}

bool FileIterator::isValid() const
{
    return !k.isEmpty();
}

FileDB::FileDB()
    : mMode(ReadOnly)
{
}

FileDB::~FileDB()
{
}

bool FileDB::isOpened() const
{
    return !mPath.isEmpty();
}

FileDB::iterator* FileDB::createIterator(ConnectionType type) const
{
    if (!isOpened())
        return 0;
    return new FileIterator(type, mMode, mPath);
}

bool FileDB::openDatabase(const Path &db, Mode mode)
{
    struct stat st;
    if (::stat(db.constData(), &st) == -1) {
        if (errno != ENOENT || mode == ReadOnly)
            return false;
        if (::mkdir(db.constData(), S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) == -1)
            return false;

        mPath = db;
        mMode = mode;
        return true;
    }

    if (!S_ISDIR(st.st_mode))
        return false;

    mPath = db;
    mMode = mode;
    return true;
}

void FileDB::closeDatabase()
{
    mPath.clear();
}

Connection* FileDB::createConnection(ConnectionType type)
{
    if (!isOpened())
        return 0;
    return new FileConnection(type, mMode, mPath);
}

static QDataStream& operator<<(QDataStream& s, const FileIndex::Entry& e)
{
    s << e.offset << e.size;
    return s;
}

static QDataStream& operator>>(QDataStream& s, FileIndex::Entry& e)
{
    s >> e.offset >> e.size;
    return s;
}
