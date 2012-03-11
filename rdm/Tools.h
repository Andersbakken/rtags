#ifndef Tools_h
#define Tools_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "UnitCache.h"
#include "Database.h"
#include <leveldb/db.h>

static inline QByteArray eatString(CXString str)
{
    const QByteArray ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

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

struct Location {
    Location() : line(0), column(0) {}

    Path path;
    int line, column;
};

static inline bool makeLocation(const QByteArray &arg, Location *loc,
                                QByteArray *resolvedLocation = 0, const Path &cwd = Path())
{
    Q_ASSERT(!arg.isEmpty());
    int colon = arg.lastIndexOf(':');
    if (colon == arg.size() - 1)
        colon = arg.lastIndexOf(':', colon - 1);
    if (colon == -1) {
        return false;
    }
    const unsigned column = atoi(arg.constData() + colon + 1);
    if (!column)
        return false;
    colon = arg.lastIndexOf(':', colon - 1);
    if (colon == -1)
        return false;
    const unsigned line = atoi(arg.constData() + colon + 1);
    if (!line)
        return false;
    const Path path = Path::resolved(arg.left(colon), cwd);
    if (path.isEmpty())
        return false;
    if (resolvedLocation)
        *resolvedLocation = path + arg.mid(colon);
    if (loc) {
        loc->line = line;
        loc->column = column;
        loc->path = path;
    }
    return true;
}

static inline void debugCursor(CXCursor c)
{
    CXSourceLocation loc = clang_getCursorLocation(c);
    CXFile file;
    unsigned int line, col;
    clang_getSpellingLocation(loc, &file, &line, &col, 0);
    CXString fn = clang_getFileName(file);
    qDebug() << Path::resolved(clang_getCString(fn)) << line << col;
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
    qDebug() << "looking at" << fileName;

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
        qDebug() << "about to visit" << inc;

        CachedUnit unit(inc, mode);
        if (unit.unit()) {
            if ((*visitor)(unit.unit(), data))
                break;
        } else {
            qWarning("Unit not found: %s", inc.constData());
        }
    }
}

#endif
