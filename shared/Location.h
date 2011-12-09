#ifndef Location_h
#define Location_h

#include <QtCore>
#include <leveldb/db.h>
#include <Path.h>
#include <RTags.h>

struct Location
{
    static QHash<Path, unsigned> *&files()
    {
        static QHash<Path, unsigned> *sFiles = 0;
        return sFiles;
    }

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
    inline QByteArray key() const // this one should only be used in debug
    {
        if (!file)
            return QByteArray();
        enum { BufSize = 256 };
        char buf[BufSize];
#ifdef QT_DEBUG
        if (files()) {
            for (QHash<Path, unsigned>::const_iterator it = files()->begin(); it != files()->end(); ++it) {
                if (it.value() == file) {
                    const int ret = snprintf(buf, BufSize, "%s:%d:%d:", it.key().constData(), line, column);
                    return QByteArray(buf, ret);
                }
            }
        }
#endif
        const int ret = snprintf(buf, BufSize, "%d:%d:%d:", file, line, column);
        return QByteArray(buf, ret);
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
