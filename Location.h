#ifndef Location_h
#define Location_h

#include "Path.h"
#include "Utils.h"
#include <clang-c/Index.h>

struct Location {
    Location()
        : line(0), column(0), offset(0)
    {}
    Location(const Path &p, unsigned l, unsigned c)
        : path(p), line(l), column(c), offset(0)
    {}
    Location(CXCursor cursor)
        : line(0), column(0), offset(0)
    {
        // ### should probably keep CXSourceLocation around rather than eating the string
        CXSourceLocation location = clang_getCursorLocation(cursor);
        CXFile file = 0;
        clang_getInstantiationLocation(location, &file, &line, &column, &offset);
        bool ok;
        path = Path::resolved(eatString(clang_getFileName(file)), Path(), &ok);
        if (!ok || !path.exists()) {
            line = column = offset = 0;
        }
    }

    bool exists() const
    {
        return path.exists();
    }

    bool isNull() const
    {
        return path.isEmpty();
    }

    QByteArray toString() const
    {
        QByteArray buffer;
        buffer.resize(path.size() + 16);
        int len = snprintf(buffer.data(), 1023, "%s:%d:%d", path.constData(), line, column);
        Q_ASSERT(len < buffer.size());
        buffer.truncate(len + 1);
        return buffer;
    }

    Path path;
    unsigned line, column, offset;
};

static inline bool operator==(const Location &l, const Location &r)
{
    return (l.line == r.line && l.column == r.column && l.path == r.path);
}

static inline bool operator!=(const Location &l, const Location &r)
{
    return (l.line != r.line || l.column != r.column || l.path != r.path);
}

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    if (!loc.exists()) {
        dbg << "Location(null)";
    } else {
        dbg << QString("Location(%1:%2:%3)").
            arg(QString::fromLocal8Bit(loc.path)).arg(loc.line).arg(loc.column);
    }
    return dbg;
}


#endif
