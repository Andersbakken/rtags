#ifndef Tools_h
#define Tools_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "UnitCache.h"
#include "Database.h"
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
