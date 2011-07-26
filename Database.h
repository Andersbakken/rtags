#ifndef Database_h
#define Database_h

#include <QtCore>
#include <QtSql>
#include <sys/stat.h>
#include <clang-c/Index.h>
#include "Utils.h"

struct Location {
    Location()
        : line(0), column(0)
    {}
    Location(CXCursor cursor)
        : line(0), column(0)
    {
        CXSourceLocation location = clang_getCursorLocation(cursor);
        CXFile file;
        clang_getInstantiationLocation(location, &file, &line, &column, 0);
        fileName = eatString(clang_getFileName(file));
        Q_ASSERT(fileName.isEmpty() == (line == 0 && column == 0));
    }

    bool exists() const
    {
        bool ret = false;
        if (!fileName.isEmpty()) {
            // ### symlinks?
            struct stat st;
            ret = !stat(fileName.constData(), &st) && S_ISREG(st.st_mode);
        }
        return ret;
    }

    QByteArray fileName;
    unsigned line, column;
};

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    if (!loc.exists()) {
        dbg << "Location(null)";
    } else {
        dbg << QString("Location(%1:%2(%3))").
            arg(QString::fromLocal8Bit(loc.fileName)).arg(loc.line).arg(loc.column);
    }
    return dbg;
}

class Database
{
public:
    static void setSymbolDefinition(const QByteArray &symbolName, const Location &location);
    static void setSymbolDeclaration(const QByteArray &symbolName, const Location &location);
    static void addSymbolReference(const QByteArray &symbolName, const Location &location);
    static Location lookupDeclaration(const QByteArray &symbolName);
    static Location lookupDefinition(const QByteArray &symbolName);
    static QList<Location> lookupReferences(const QByteArray &symbolName);
};

#endif
