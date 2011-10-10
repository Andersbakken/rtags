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
    Location(const char *locationString)
        : line(0), column(0), offset(0)
    {
        const int len = strlen(locationString);
        unsigned *ptrs[] = { &column, &line, 0u };
        int idx = 0;
        for (int i=len - 1; i>0; --i) {
            if (locationString[i] == ':') {
                *ptrs[idx] = atoi(locationString + i + 1);
#ifdef QT_DEBUG
                if (!*ptrs[idx])
                    qWarning("%s %s %d\n", locationString, locationString + i, i);
#endif
                Q_ASSERT(*ptrs[idx]);
                if (!ptrs[++idx]) {
                    path = QByteArray(locationString, i);
                    break;
                }
            }
        }
#ifdef QT_DEBUG
        if (!column || !line)
            qWarning("%s\n", locationString);
#endif
        Q_ASSERT(column && line);
    }
    Location(CXCursor cursor)
        : line(0), column(0), offset(0)
    {
        // ### should probably keep CXSourceLocation around rather than eating the string
        CXSourceLocation location = clang_getCursorLocation(cursor);
        CXFile file = 0;
        clang_getExpansionLocation(location, &file, &line, &column, &offset);
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
        if (!path.isEmpty()) {
            buffer.resize(path.size() + 16);
            int len = snprintf(buffer.data(), 1023, "%s:%d:%d", path.constData(), line, column);
            Q_ASSERT(len < buffer.size());
            buffer.truncate(len);
        }
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
