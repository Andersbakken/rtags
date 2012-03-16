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

static inline QByteArray eatString(CXString str)
{
    const QByteArray ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

static inline void debugCursor(CXCursor c)
{
    CXSourceLocation loc = clang_getCursorLocation(c);
    CXFile file;
    unsigned int line, col;
    clang_getSpellingLocation(loc, &file, &line, &col, 0);
    CXString fn = clang_getFileName(file);
    log(1) << Path::resolved(clang_getCString(fn)) << line << col;
    clang_disposeString(fn);
}

struct FirstUnitData
{
    FirstUnitData() : data(0) { }
    ~FirstUnitData() { UnitCache::instance()->release(data); }

    QByteArray fileName;
    UnitCache::Unit* data;
};

static inline bool visitFindFirstUnit(UnitCache::Unit* ud, void* data)
{
    FirstUnitData* firstdata = static_cast<FirstUnitData*>(data);

    CXFile file = clang_getFile(ud->unit, firstdata->fileName.constData());
    if (!file)
        return false;

    firstdata->data = UnitCache::instance()->acquire(ud->fileName); // add a new ref
    return true;
}

typedef bool (*VisitFile)(UnitCache::Unit* unit, void* data);

static inline void visitIncluderFiles(const QByteArray& fileName, VisitFile visitor, void* data,
                                      int mode = UnitCache::AST | UnitCache::Memory)
{
    warning() << "looking at" << fileName;

    QByteArray dbname = Database::databaseName(Database::Include);
    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), dbname.constData(), &db);
    if (!status.ok())
        return;
    std::string value;
    db->Get(leveldb::ReadOptions(), fileName.constData(), &value);
    delete db;
    if (value.empty())
        return;

    QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
    QList<QByteArray> others = bvalue.split('\n');
    foreach(const QByteArray& inc, others) {
        if (inc.isEmpty())
            continue;
        warning() << "about to visit" << inc;

        CachedUnit unit(inc, mode);
        if (unit.unit()) {
            if ((*visitor)(unit.unit(), data))
                break;
        } else {
            warning("Unit not found: %s", inc.constData());
        }
    }
}

enum MakeLocationFlag {
    IncludeContext = 0x1
};
static inline QByteArray cursorToString(CXCursor cursor)
{
    QByteArray ret = eatString(clang_getCursorKindSpelling(clang_getCursorKind(cursor)));
    const QByteArray name = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty())
        ret += " " + name;

    CXFile file;
    unsigned line, col;
    clang_getInstantiationLocation(clang_getCursorLocation(cursor), &file, &line, &col, 0);
    const QByteArray fileName = eatString(clang_getFileName(file));
    if (!fileName.isEmpty()) {
        ret += " " + fileName + ':' + QByteArray::number(line) + ':' +  QByteArray::number(col);
    }
    return ret;
}

static inline QByteArray makeLocation(CXCursor cursor, unsigned flags = 0)
{
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned line, col, off;
    clang_getSpellingLocation(loc, &file, &line, &col, &off);
    CXString fn = clang_getFileName(file);
    QByteArray ret;
    ret.reserve(256);
    ret += clang_getCString(fn);
    clang_disposeString(fn);
    const int len = RTags::canonicalizePath(ret.data(), ret.size());
    const int extra = RTags::digits(line) + RTags::digits(col) + 2;
    const QByteArray ctx = (flags & IncludeContext ? RTags::context(ret, off, col) : QByteArray());
    if (ctx.isEmpty()) {
        ret.resize(len + extra);
        snprintf(ret.data() + len, extra + 1, ":%d:%d", line, col);
    } else {
        ret.resize(len + extra + 1 + ctx.size());
        snprintf(ret.data() + len, extra + 1 + ctx.size(), ":%d:%d\t%s", line, col, ctx.constData());
    }
    return ret;
}


#endif
