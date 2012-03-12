#ifndef Tools_h
#define Tools_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <QDebug>
#include "UnitCache.h"
#include "Database.h"
#include <leveldb/db.h>
#include <Shared.h>

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

static inline int readLine(FILE *f, char *buf, int max)
{
    assert(!buf == (max == -1));
    if (max == -1)
        max = INT_MAX;
    for (int i=0; i<max; ++i) {
        const int ch = fgetc(f);
        if (ch == '\n' || ch == EOF) {
            if (buf)
                *buf = '\0';
            return i;
        }
        if (buf)
            *buf++ = *reinterpret_cast<const char*>(&ch);
    }
    return -1;
}


static inline QByteArray context(const Path &path, unsigned offset, unsigned col)
{
    FILE *f = fopen(path.constData(), "r");
    if (f && !fseek(f, offset - (col - 1), SEEK_SET)) {
        char buf[1024] = { '\0' };
        const int len = readLine(f, buf, 1023);
        fclose(f);
        return QByteArray(buf, len);
    }
    return QByteArray();
}


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
