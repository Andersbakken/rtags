#include "FileDB.h"
#include "Mmap.h"
#include <QList>
#include <QIODevice>
#include <QDataStream>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

class FdDevice : public QIODevice
{
public:
    FdDevice(int f);

    bool isSequential() const;
    bool atEnd() const;

protected:
    qint64 readData(char *data, qint64 maxlen);
    qint64 writeData(const char *data, qint64 len);

private:
    int fd;
    bool end;
};

FdDevice::FdDevice(int f)
    : fd(f), end(false)
{
    open(QIODevice::ReadWrite);
}

bool FdDevice::isSequential() const
{
    return true; // for ease of implementation
}

bool FdDevice::atEnd() const
{
    return end;
}

qint64 FdDevice::readData(char *data, qint64 maxlen)
{
    qint64 r = ::read(fd, data, maxlen);
    if (r == 0)
        end = true;
    return r;
}

qint64 FdDevice::writeData(const char *data, qint64 len)
{
    return ::write(fd, data, len);
}

class FileIndex
{
public:
    enum { PartialMax = 16 };
    enum FindType { Exact, LowerBound, UpperBound };

    FileIndex();
    FileIndex(int i, Mmap* m);

    void init(int i, Mmap* m);
    void clear();

    void read();
    void write();

    void add(const QByteArray& key, int offset);
    int remove(const QByteArray& key);
    bool find(FindType type, const QByteArray& key, int* offset, int* size = 0);

    void maybeSort();

    struct Entry
    {
        QByteArray partial;
        int offset;
        int size;
        Mmap* mmap;

        bool operator<(const Entry& other) const;

        QByteArray key() const;
        bool isEqual(const QByteArray& key) const;
    };

    QList<Entry> sorted, unsorted;
    int idx;
    Mmap* mmap;
};

QByteArray FileIndex::Entry::key() const
{
    if (offset == -1)
        return partial;

    mmap->seek(offset);
    return mmap->get<QByteArray>();
}

bool FileIndex::Entry::isEqual(const QByteArray& key) const
{
    if (key.size() != size)
        return false;
    if (partial.size() == FileIndex::PartialMax) {
        if (key.left(FileIndex::PartialMax) != partial)
            return false;
        return (this->key() == key);
    }
    Q_ASSERT(partial.size() < FileIndex::PartialMax);
    return (partial == key);
}

bool FileIndex::Entry::operator<(const Entry& other) const
{
    const int sz = partial.size();
    const int os = other.partial.size();
    if (sz > PartialMax) {
        // lhs is a full key
        if (os != PartialMax)
            return partial < other.partial;
        const QByteArray np = partial.left(PartialMax);
        if (np < other.partial)
            return true;
        if (other.partial < np)
            return false;
        return partial < other.key();
    }

    if (os > PartialMax) {
        // rhs is a full key
        if (sz != PartialMax)
            return partial < other.partial;
        const QByteArray np = other.partial.left(PartialMax);
        if (partial < np)
            return true;
        if (np < partial)
            return false;
        return key() < other.partial;
    }

    // no full keys or both full keys with size < PartialMax
    if (sz < PartialMax || os < PartialMax)
        return partial < other.partial;
    return key() < other.key();
}

FileIndex::FileIndex()
    : idx(-1), mmap(0)
{
}

FileIndex::FileIndex(int i, Mmap* m)
{
    init(i, m);
}

void FileIndex::init(int i, Mmap* m)
{
    idx = i;
    mmap = m;
    read();
}

void FileIndex::clear()
{
    if (idx == -1)
        return;

    ::close(idx);
    idx = -1;
    mmap = 0;
    sorted.clear();
    unsorted.clear();
}

void FileIndex::read()
{
    sorted.clear();
    unsorted.clear();

    FdDevice fddev(idx);
    QDataStream stream(&fddev);
    stream >> sorted;

    for (int i = 0; i < sorted.size(); ++i) {
        sorted[i].mmap = mmap;
    }
}

void FileIndex::maybeSort()
{
    if (unsorted.isEmpty())
        return;
    sorted += unsorted;
    unsorted.clear();
    qSort(sorted);
}

void FileIndex::write()
{
    maybeSort();

    FdDevice fddev(idx);
    QDataStream stream(&fddev);
    stream << sorted;
}

void FileIndex::add(const QByteArray &key, int offset)
{
    enum { SortThreshold = 1000 }; // number of entries 'unsorted' can hold before getting sorted

    Entry e;
    e.size = key.size();
    e.mmap = mmap;

    if (!unsorted.isEmpty()) {
        if (unsorted.size() >= SortThreshold) {
            maybeSort();
            Q_ASSERT(unsorted.isEmpty());
        }

        QList<Entry>::iterator i = unsorted.begin();
        const QList<Entry>::const_iterator end = unsorted.end();
        while (i != end) {
            if ((*i).isEqual(key)) {
                Entry& found = *i;

                // 'erase' the existing key in the mmap data
                mmap->seek(found.offset);
                mmap->set(QByteArray(1, '\0'));

                found.offset = offset;
                Q_ASSERT(found.size == e.size && found.mmap == e.mmap);
                return;
            }
            ++i;
        }
    }
    if (!sorted.isEmpty()) {
        e.partial = key;
        e.offset = -1;

        QList<Entry>::iterator i = qBinaryFind(sorted.begin(), sorted.end(), e);
        if (i != sorted.end()) {
            Entry& found = *i;

            // 'erase' the existing key in the mmap data
            mmap->seek(found.offset);
            mmap->set(QByteArray(1, '\0'));

            found.offset = offset;
            Q_ASSERT(found.size == e.size && found.mmap == e.mmap);
            return;
        }
    }

    e.offset = offset;
    e.partial = key.left(PartialMax);
    unsorted.append(e);
}

bool FileIndex::find(FindType type, const QByteArray &key, int *offset, int *size)
{
    maybeSort();

    Entry find;
    find.partial = key;
    find.offset = -1;
    find.mmap = 0;
    find.size = key.size();

    QList<Entry>::const_iterator i;
    switch (type) {
    case LowerBound:
        i = qLowerBound(sorted.begin(), sorted.end(), find);
        break;
    case UpperBound:
        i = qUpperBound(sorted.begin(), sorted.end(), find);
        break;
    default:
        i = qBinaryFind(sorted.begin(), sorted.end(), find);
        break;
    }

    if (i == sorted.end())
        return false;
    *offset = (*i).offset;
    if (size)
        *size = (*i).size;
    return true;
}

int FileIndex::remove(const QByteArray &key)
{
    maybeSort();

    Entry find;
    find.partial = key;
    find.offset = -1;
    find.mmap = 0;
    find.size = key.size();

    QList<Entry>::iterator i = qBinaryFind(sorted.begin(), sorted.end(), find);
    if (i == sorted.end())
        return -1;
    const int at = (*i).offset;
    sorted.erase(i);
    return at;
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
    mutable Mmap mmap;
    mutable FileIndex idx;
};

static int modeToFlags(FileDB::Mode mode)
{
    switch (mode) {
    case FileDB::ReadOnly:
        return O_RDONLY;
    case FileDB::WriteOnly:
    case FileDB::ReadWrite:
        // open for RDWR even in write only since we want to read internally even
        // if reads using the external API should not be allowed
        return O_RDWR | O_CREAT;
    }
    return 0;
}

FileConnection::FileConnection(FileDB::ConnectionType t, FileDB::Mode dbm, const Path& p)
    : type(t), mode(dbm)
{
    char fn = char('a' + t);
    const int f = modeToFlags(mode);
    const int m = S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;
    int idxfd = ::open((p + "/" + fn + ".idx").constData(), f, m);
    mmap.load(p + "/" + fn + ".db");

    idx.init(idxfd, &mmap);
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
        mmap.seek(offset + size + sizeof(int));
        QByteArray v = mmap.get<QByteArray>();
        return v;
    }
    return QByteArray();
}

void FileConnection::writeData(const QByteArray &key, const QByteArray &value)
{
    Q_ASSERT(!key.isEmpty());

    if (value.isEmpty()) {
        int at = idx.remove(key);
        if (at == -1)
            return;
        mmap.seek(at);
        mmap.set(QByteArray(1, '\0'));
        return;
    }

    const unsigned int pos = mmap.size();
    mmap.seek(pos);
    const unsigned int off = mmap.offset();
    mmap.set(key);
    mmap.set(value);
    idx.add(key, off);
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
    Mmap mmap;
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
    mmap.clear(Mmap::Sync);
    idx.clear();

    char fn = char('a' + t);
    const int f = modeToFlags(mode);
    const int m = S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;
    int idxfd = ::open((path + "/" + fn + ".idx").constData(), f, m);
    mmap.load(path + "/" + fn + ".db");

    idx.init(idxfd, &mmap);

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
        mmap.seek(offset);
        next();
        return true;
    }
    return false;
}

bool FileIterator::next()
{
    bool ok;
    forever {
        k = mmap.get<QByteArray>(&ok);
        if (!ok) {
            k.clear();
            v.clear();
            return false;
        }
        v = mmap.get<QByteArray>();

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
    s << e.partial << e.offset << e.size;
    return s;
}

static QDataStream& operator>>(QDataStream& s, FileIndex::Entry& e)
{
    s >> e.partial >> e.offset >> e.size;
    return s;
}
