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
        if (fileName.isEmpty()) { // This seems to happen
            line = column = 0 ;
        } else {
            resolvePath(fileName);
        }
        Q_ASSERT(fileName.isEmpty() == (line == 0 && column == 0));
    }

    bool exists() const
    {
        return fileExists(fileName);
    }

    QByteArray fileName;
    unsigned line, column;
};

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    if (!loc.exists()) {
        dbg << "Location(null)";
    } else {
        dbg << QString("Location(%1:%2:%3)").
            arg(QString::fromLocal8Bit(loc.fileName)).arg(loc.line).arg(loc.column);
    }
    return dbg;
}

class Database
{
public:
    static void clear();

    static void addFile(const QByteArray &file, const QList<QByteArray> &compilerOptions);
    static bool removeFile(const QByteArray &file);
    static QList<QByteArray> compilerOptions(const QByteArray &absoluteFilePath);
    static QList<QByteArray> takeCompilerOptions(const QByteArray &absoluteFilePath);

    static void removeReferences(const QByteArray &absoluteFilePath);
    static void setSymbolDeclaration(const QByteArray &symbolName, const Location &location);
    static bool clearSymbolDeclaration(const QByteArray &symbolName);
    static Location lookupDeclaration(const QByteArray &symbolName);
    static int symbolDeclarationSize();

    static void setSymbolDefinition(const QByteArray &symbolName, const Location &location);
    static Location lookupDefinition(const QByteArray &symbolName);
    static bool clearSymbolDefinition(const QByteArray &symbolName);
    static int symbolDefinitionSize();

    static void addSymbolReference(const QByteArray &symbolName, const Location &location);
    static QList<Location> lookupReferences(const QByteArray &symbolName);
    static int clearSymbolReferences(const QByteArray &symbolName);
    static int symbolReferencesSize();

};

#endif
