#include "FileDB.h"
#include <QVector>
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
    enum { PartialMax = 4 };

    FileIndex();
    FileIndex(int i, int d);

    void init(int i, int d);
    void clear();

    void read();
    void write();

    void add(const QByteArray& key, int offset);
    bool find(const QByteArray& key, int* offset, int* size = 0);
    bool lowerBound(const QByteArray& key, int* offset, int* size = 0);

    void maybeSort();

    struct Entry
    {
        QByteArray partial;
        int offset;
        int size;
        int db;

        bool operator<(const Entry& other) const;

        QByteArray key() const;
    };

    QVector<Entry> entries;
    int idx, db;
    bool dirty;
};

QByteArray FileIndex::Entry::key() const
{
    if (offset == -1)
        return partial;

    ::lseek(db, offset, SEEK_SET);
    QByteArray k(size, '\0');
    ssize_t r = ::read(db, k.data(), size);
    Q_ASSERT(r == size);
    return k;
}

bool FileIndex::Entry::operator<(const Entry& other) const
{
    if (partial.size() > PartialMax)
        return partial < other.key();
    else if (other.partial.size() > PartialMax)
        return key() < other.partial;
    else if (partial < other.partial)
        return true;
    else if (partial > other.partial)
        return false;
    return key() < other.key();
}

FileIndex::FileIndex()
    : idx(-1), db(-1), dirty(false)
{
}

FileIndex::FileIndex(int i, int d)
{
    init(i, d);
}

void FileIndex::init(int i, int d)
{
    idx = i;
    db = d;
    dirty = false;
    read();
}

void FileIndex::clear()
{
    Q_ASSERT(idx != -1);

    ::close(idx);
    dirty = false;
    idx = -1;
    db = -1;
    entries.clear();
}

void FileIndex::read()
{
    entries.clear();

    FdDevice fddev(idx);
    QDataStream stream(&fddev);
    stream >> entries;

    for (int i = 0; i < entries.size(); ++i) {
        entries[i].db = db;
    }
}

void FileIndex::write()
{
    qSort(entries);
    dirty = false;

    FdDevice fddev(idx);
    QDataStream stream(&fddev);
    stream << entries;
}

void FileIndex::add(const QByteArray &key, int offset)
{
    Entry e;
    e.partial = key.left(PartialMax);
    e.offset = offset;
    e.size = key.size();
    e.db = db;
    entries.append(e);
    dirty = true;
}

void FileIndex::maybeSort()
{
    if (!dirty)
        return;
    qSort(entries);
    dirty = false;
}

bool FileIndex::find(const QByteArray &key, int *offset, int *size)
{
    maybeSort();

    Entry find;
    find.partial = key;
    find.offset = -1;
    find.db = -1;
    find.size = key.size();

    QVector<Entry>::const_iterator i = qBinaryFind(entries, find);
    if (i == entries.end())
        return false;
    *offset = (*i).offset;
    if (size)
        *size = (*i).size;
    return true;
}

bool FileIndex::lowerBound(const QByteArray &key, int *offset, int *size)
{
    maybeSort();

    Entry find;
    find.partial = key;
    find.offset = -1;
    find.db = -1;
    find.size = key.size();

    QVector<Entry>::const_iterator i = qLowerBound(entries, find);
    if (i == entries.end())
        return false;
    *offset = (*i).offset;
    if (size)
        *size = (*i).size;
    return true;
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
    int db;
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
    db = ::open((p + "/" + fn + ".db").constData(), f, m);

    idx.init(idxfd, db);
}

FileConnection::~FileConnection()
{
    if (mode != FileDB::ReadOnly)
        idx.write();
}

QByteArray FileConnection::readData(const QByteArray &key) const
{
    int offset, size;
    if (idx.find(key, &offset, &size)) {
        if (size == 0)
            return QByteArray();
        QByteArray value(size, '\0');
        ::lseek(db, offset, SEEK_SET);
        ssize_t r = ::read(db, value.data(), size);
        Q_ASSERT(r == size);
        return value;
    }
    return QByteArray();
}

void FileConnection::writeData(const QByteArray &key, const QByteArray &value)
{
    off_t pos = ::lseek(db, 0, SEEK_END);
    if (pos == -1) // ouch
        return;
    int sz = key.size();
    ssize_t w = ::write(db, &sz, sizeof(int));
    Q_ASSERT(w == sizeof(int));
    w = ::write(db, key.constData(), sz);
    Q_ASSERT(w == sz);
    sz = value.size();
    w = ::write(db, &sz, sizeof(int));
    Q_ASSERT(w == sizeof(int));
    w = ::write(db, value.constData(), sz);
    Q_ASSERT(w == sz);
    idx.add(key, pos + sizeof(int));
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
    int db;
    int pos;
    bool all;
    FileDB::Mode mode;
    FileDB::ConnectionType current;

    QByteArray k, v;
};

FileIterator::FileIterator(FileDB::ConnectionType t, FileDB::Mode m, const Path& p)
    : Database::iterator(t), path(p), db(-1), pos(0), all(false), mode(m)
{
    if (t == FileDB::All) {
        all = true;
        current = FileDB::General;
        open(current);
    } else {
        current = t;
        open(t);
    }
}

void FileIterator::open(FileDB::ConnectionType t)
{
    if (db != -1) {
        ::close(db);
        idx.clear();
    }

    pos = 0;

    char fn = char('a' + t);
    const int f = modeToFlags(mode);
    const int m = S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;
    int idxfd = ::open((path + "/" + fn + ".idx").constData(), f, m);
    db = ::open((path + "/" + fn + ".db").constData(), f, m);

    idx.init(idxfd, db);

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
    if (all)
        qFatal("FIXME: FileIterator::seek() does not work with Database::All");

    int offset;
    if (idx.lowerBound(key, &offset)) {
        offset -= sizeof(int);
        ::lseek(db, offset, SEEK_SET);
        pos = offset;
        next();
    }
    return false;
}

bool FileIterator::next()
{
    ::lseek(db, pos, SEEK_SET); // ### how efficient is this if the current position already is at 'pos'?

    int sz;
    ssize_t r = ::read(db, &sz, sizeof(int));
    if (r < (int)sizeof(int)) {
        if (all && current < FileDB::NumConnections - 1) {
            current = static_cast<FileDB::ConnectionType>(current + 1);
            open(current);
            if (!isValid())
                return next();
            return true;
        }

        k.clear();
        v.clear();
        return false;
    }
    Q_ASSERT(r == sizeof(int));
    k.resize(sz);
    r = ::read(db, k.data(), sz);
    Q_ASSERT(r == sz);
    pos += sizeof(int) + sz;

    r = ::read(db, &sz, sizeof(int));
    Q_ASSERT(r == sizeof(int));
    v.resize(sz);
    r = ::read(db, v.data(), sz);
    Q_ASSERT(r == sz);
    pos += sizeof(int) + sz;

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
