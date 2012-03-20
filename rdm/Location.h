#ifndef Location_h
#define Location_h

#include <QtCore>
#include <leveldb/db.h>
#include <Path.h>
#include <RTags.h>

struct Location
{
    Location()
        : file(0), line(0), column(0)
    {}

    static Location fromKey(const QByteArray &key)
    {
        unsigned ints[3] = { -1, -1, -1 };
        const char *data = key.constData();
        for (int i=0; i<3; ++i) {
            char *end = 0;
            unsigned val = strtoul(data, &end, 10);
            if (!val || (i < 2 && !end))
                return Location();
            ints[i] = val;
            if (end) {
                data = ++end;
            }
        }
        Location loc;
        loc.file = ints[0];
        loc.line = ints[1];
        loc.column = ints[2];
        return loc;
    }

    unsigned file, line, column;
    inline bool operator==(const Location &other) const
    {
        return file == other.file && line == other.line && column == other.column;
    }
    inline bool operator<(const Location &other) const
    {
        if (file < other.file)
            return true;
        if (file > other.file)
            return false;
        if (line < other.line)
            return true;
        if (line > other.line)
            return false;
        if (column > other.column)
            return false;
        return true;
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
    if (!location.file) {
        dbg << "Location()";
        return dbg;
    }
    enum { BufSize = 1024 };
    char buf[BufSize];
#if 0
    extern QHash<Path, unsigned> *filesByNameDebugUgleHack;
    if (filesByNameDebugUgleHack) {
        for (QHash<Path, unsigned>::const_iterator it = filesByNameDebugUgleHack->begin();
             it != filesByNameDebugUgleHack->end(); ++it) {
            if (it.value() == location.file) {
                const int ret = snprintf(buf, BufSize, "%s:%d:%d:", it.key().constData(),
                                         location.line, location.column);
                dbg << QByteArray::fromRawData(buf, ret);
                return dbg;
            }
        }
    }
#endif
    const int ret = snprintf(buf, BufSize, "%d:%d:%d:", location.file, location.line, location.column);
    dbg << QByteArray::fromRawData(buf, ret);
    return dbg;
}

#endif
