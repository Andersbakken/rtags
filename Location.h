#ifndef Location_h
#define Location_h

#include "Path.h"
#include "Utils.h"
#include <clang-c/Index.h>

struct Location {
    Location()
        : line(0), column(0)
    {}
    Location(const Path &p, unsigned l, unsigned c)
        : path(p), line(l), column(c)
    {}
    Location(CXCursor cursor)
        : line(0), column(0)
    {
        CXSourceLocation location = clang_getCursorLocation(cursor);
        CXFile file;
        clang_getInstantiationLocation(location, &file, &line, &column, 0);
        bool ok;
        path = Path::resolved(eatString(clang_getFileName(file)), &ok);
        if (!ok) {
            line = column = 0 ;
        }
        Q_ASSERT((!path.exists()) == (line == 0 && column == 0));
    }

    bool exists() const
    {
        return path.exists();
    }

    QByteArray toString() const
    {
        QByteArray buffer;
        buffer.resize(1024);
        int len = snprintf(buffer.data(), 1023, "%s:%d:%d", path.constData(), line, column);
        buffer.truncate(len + 1);
        return buffer;
    }

    Path path;
    unsigned line, column;
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
