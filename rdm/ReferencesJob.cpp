#include "ReferencesJob.h"
#include "Database.h"
#include <leveldb/db.h>
#include "UnitCache.h"
#include "Tools.h"

ReferencesJob::ReferencesJob(const QByteArray& fn, int i, int l, int c)
    : fileName(fn), id(i), line(l), col(c)
{
}

ReferencesJob::~ReferencesJob()
{
}

static inline void readReferences(const char* usr, QList<QByteArray>& refs)
{
    QByteArray fileName = Database::databaseName(Database::Reference);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), fileName.constData(), &db);
    if (!status.ok())
        return;

    std::string value;
    db->Get(leveldb::ReadOptions(), usr, &value);
    if (value.empty()) {
        delete db;
        return;
    }

    QByteArray qvalue = QByteArray::fromRawData(value.c_str(), value.size());
    refs = qvalue.split('\n');

    delete db;
}

void ReferencesJob::run()
{
    if (line == -1 && col == -1)
        runName();
    else
        runLocation();
}

void ReferencesJob::runName()
{
    QByteArray databasename = Database::databaseName(Database::Symbol);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), databasename.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    std::string value;
    db->Get(leveldb::ReadOptions(), fileName.constData(), &value);
    if (value.empty()) {
        delete db;
        emit complete(id, QList<QByteArray>());
        return;
    }

    QByteArray bvalue = QByteArray::fromRawData(value.c_str(), value.size());
    QList<QByteArray> list = bvalue.split('\n');
    //log(1) << "runName result" << list;

    delete db;

    databasename = Database::databaseName(Database::Definition);
    status = leveldb::DB::Open(leveldb::Options(), databasename.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    QList<QByteArray> result;
    foreach(const QByteArray& entry, list) {
        if (entry.isEmpty())
            continue;
        // if the entry looks like a path, return it outright
        if (entry.at(0) == '/') {
            result.append(entry);
            continue;
        }
        db->Get(leveldb::ReadOptions(), entry.constData(), &value);
        if (value.empty())
            continue;
        bvalue = QByteArray::fromRawData(value.c_str(), value.size());
        foreach(const QByteArray& line, bvalue.split('\n')) {
            if (!line.isEmpty())
                result.append(line);
        }
    }

    delete db;
    emit complete(id, result);
}

void ReferencesJob::runLocation()
{
    CachedUnit locker(fileName, UnitCache::AST | UnitCache::Memory);
    UnitCache::Unit* data = locker.unit();
    if (!data) {
        FirstUnitData first;
        first.fileName = fileName;
        visitIncluderFiles(fileName, visitFindFirstUnit, &first);
        if (first.data) {
            locker.adopt(first.data);
            data = first.data;
        } else {
            log("references: no unit for %s", fileName.constData());
            emit complete(id, QList<QByteArray>());
            return;
        }
    }

    CXTranslationUnit unit = data->unit;
    CXFile file = data->file;

    CXSourceLocation loc = clang_getLocation(unit, file, line, col);
    CXCursor cursor = clang_getCursor(unit, loc);
    CXCursor def = clang_getCursorDefinition(cursor);
    if (clang_equalCursors(def, clang_getNullCursor())) {
        CXCursor ref = clang_getCursorReferenced(cursor);
        if (clang_equalCursors(ref, clang_getNullCursor())) {
            log("no ref and no def!");
            emit complete(id, QList<QByteArray>());
            return;
        }

        CXString usr = clang_getCursorUSR(ref);
        if (!clang_getCString(usr) || !strcmp(clang_getCString(usr), "")) {
            clang_disposeString(usr);
            log() << "no usr";
            emit complete(id, QList<QByteArray>());
            return;
        }

        QList<QByteArray> refs;
        readReferences(clang_getCString(usr), refs);
        emit complete(id, refs);
        return;
    }

    CXString usr = clang_getCursorUSR(def);
    if (!clang_getCString(usr) || !strcmp(clang_getCString(usr), "")) {
        clang_disposeString(usr);
        log() << "no usr";
        emit complete(id, QList<QByteArray>());
        return;
    }

    QList<QByteArray> refs;
    readReferences(clang_getCString(usr), refs);
    clang_disposeString(usr);
    emit complete(id, refs);
}
