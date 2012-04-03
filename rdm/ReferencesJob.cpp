#include "ReferencesJob.h"
#include "Database.h"
#include <leveldb/db.h>
#include "Rdm.h"

ReferencesJob::ReferencesJob(int i, const RTags::Location &loc, bool ctx)
    : id(i), location(loc), symbolName(QByteArray()), includeContext(ctx)
{
}

ReferencesJob::ReferencesJob(int i, const QByteArray &sym, bool ctx)
    : id(i), location(RTags::Location()), symbolName(sym), includeContext(ctx)
{
}

void ReferencesJob::run()
{
    if (!symbolName.isEmpty()) {
        const QByteArray name = Database::databaseName(Database::Symbol);
        leveldb::DB* db = 0;
        leveldb::Status status = leveldb::DB::Open(leveldb::Options(), name.constData(), &db);
        if (!status.ok()) {
            emit complete(id, QList<QByteArray>());
            return;
        }

        std::string value;
        db->Get(leveldb::ReadOptions(), symbolName.constData(), &value);
        delete db;
        if (value.empty()) {
            emit complete(id, QList<QByteArray>());
            return;
        }
        location = RTags::Location::fromKey(QByteArray::fromRawData(value.c_str(), value.size()));
    }
    leveldb::DB* db = 0;
    leveldb::Options options;
    const QByteArray name = Database::databaseName(Database::Symbol);
    if (name.isEmpty()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    leveldb::Status status = leveldb::DB::Open(options, name.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }
    Q_ASSERT(db);
    Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db, location);
    QList<QByteArray> list;
    QSet<RTags::Location> refs = cursorInfo.references;
    if (refs.isEmpty() && !cursorInfo.target.isNull()) {
        cursorInfo = Rdm::findCursorInfo(db, cursorInfo.target);
        refs = cursorInfo.references;
    }
    foreach (const RTags::Location &loc, cursorInfo.references) {
        list.append(loc.key(includeContext ? RTags::Location::ShowContext : RTags::Location::NoFlag));
    }
    emit complete(id, list);
}
