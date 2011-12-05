#ifndef Location_h
#define Location_h

#include <QtCore>
#include <AtomicString.h>
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

    unsigned file, line, column;
    inline QByteArray key() const // this one should only be used in debug
    {
        if (!file)
            return QByteArray();
        char buf[1024];
        QByteArray fn;
        fn = QByteArray::number(file);
        if (files()) {
            for (QHash<Path, unsigned>::const_iterator it = files()->begin(); it != files()->end(); ++it) {
                if (it.value() == file) {
                    fn = it.key();
                    break;
                }
            }
        }

        const int ret = snprintf(buf, 1024, "%s:%d:%d:", fn.constData(), line, column);
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
