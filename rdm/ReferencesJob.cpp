#include "ReferencesJob.h"
#include "Database.h"
#include "LevelDB.h"
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
    LevelDB db;
    if (!symbolName.isEmpty()) {
        if (!db.open(Database::Symbol, LevelDB::ReadOnly)) {
            emit complete(id, QList<QByteArray>());
            return;
        }
        std::string value;
        db.db()->Get(leveldb::ReadOptions(), symbolName.constData(), &value);
        if (value.empty()) {
            emit complete(id, QList<QByteArray>());
            return;
        }
        location = RTags::Location::fromKey(QByteArray::fromRawData(value.c_str(), value.size()));
    }
    if (!db.db() && !db.open(Database::Symbol, LevelDB::ReadOnly)) {
        emit complete(id, QList<QByteArray>());
        return;
    }
    Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db.db(), location);
    QList<QByteArray> list;
    QSet<RTags::Location> refs = cursorInfo.references;
    if (refs.isEmpty() && !cursorInfo.target.isNull()) {
        cursorInfo = Rdm::findCursorInfo(db.db(), cursorInfo.target);
        refs = cursorInfo.references;
    }
    foreach (const RTags::Location &loc, cursorInfo.references) {
        list.append(loc.key(includeContext ? RTags::Location::ShowContext : RTags::Location::NoFlag));
    }
    emit complete(id, list);
}
