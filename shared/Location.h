#ifndef Location_h
#define Location_h

#include <QtCore>
#include <AtomicString.h>
#include <leveldb/db.h>
#include <Path.h>
#include <RTags.h>

struct Location
{
    static QHash<Path, unsigned> *sFiles;
    Location()
        : file(0), line(0), column(0)
    {}

    unsigned file, line, column;
    inline AtomicString fileName() const
    {
        if (file) {
            for (QHash<Path, unsigned>::const_iterator it = sFiles->begin(); it != sFiles->end(); ++it) {
                if (it.value() == file)
                    return it.key();
            }
        }
        return AtomicString();
    }
    inline QByteArray key() const
    {
        if (!file)
            return QByteArray();
        char buf[1024];
        const int ret = snprintf(buf, 1024, "%s:%d:%d", fileName().constData(), line, column);
        return QByteArray(buf, ret);
    }
    inline QByteArray key(leveldb::DB *db) const
    {
        QByteArray ret;
        if (file) {
            char buf[32];
            snprintf(buf, 32, "F:%d", file);
            if (RTags::readFromDB(db, buf, ret)) {
                snprintf(buf, 32, ":%d:%d:", line, column);
                ret += buf;
            }
        }
        return ret;
    }
    inline bool operator==(const Location &other) const
    {
        return file == other.file && line == other.line && column == other.column;
    }
};

static inline uint qHash(const Location &l)
{
    // ### is this good?
    return (l.file << 1) + (l.line << 2) + (l.column << 3);
}

static inline QDataStream &operator<<(QDataStream &ds, const Location &loc)
{
    ds << loc.file << loc.line << loc.column;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Location &loc)
{
    ds >> loc.file >> loc.line >> loc.column;
    return ds;
}


static inline QDebug operator<<(QDebug dbg, const Location &location)
{
    dbg << (location.file ? location.key() : QByteArray("Location()"));
    return dbg;
}

#endif
