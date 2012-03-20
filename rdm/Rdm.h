#ifndef Tools_h
#define Tools_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "UnitCache.h"
#include "RDatabase.h"
#include <leveldb/db.h>
#include <RTags.h>

namespace Rdm {
QByteArray eatString(CXString str);
void debugCursor(CXCursor c);
struct FirstUnitData
{
    FirstUnitData();
    ~FirstUnitData();

    QByteArray fileName;
    UnitCache::Unit* data;
};

class String
{
    String(const String &other);
    String &operator=(const String &other);
public:
    String(CXString s)
        : str(s)
    {}

    ~String()
    {
        clang_disposeString(str);
    }
    const char *data() const
    {
        return clang_getCString(str);
    }

    CXString str;
};


bool visitFindFirstUnit(UnitCache::Unit* ud, void* data);
typedef bool (*VisitFile)(UnitCache::Unit* unit, void* data);
void visitIncluderFiles(const QByteArray& fileName, VisitFile visitor, void* data,
                        int mode = UnitCache::AST | UnitCache::Memory);
enum MakeLocationFlag {
    IncludeContext = 0x1
};
QByteArray cursorToString(CXCursor cursor);
QByteArray makeLocation(CXCursor cursor, unsigned flags = 0);
}

#endif
