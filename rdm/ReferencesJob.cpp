#include "ReferencesJob.h"
#include "Database.h"
#include "LevelDB.h"
#include "Rdm.h"

ReferencesJob::ReferencesJob(int i, const RTags::Location &loc, bool ctx)
    : id(i), symbolName(QByteArray()), includeContext(ctx)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(int i, const QByteArray &sym, bool ctx)
    : id(i), symbolName(sym), includeContext(ctx)
{
}

void ReferencesJob::run()
{
    LevelDB db;
    if (!symbolName.isEmpty()) {
        if (!db.open(Database::SymbolName, LevelDB::ReadOnly)) {
            emit complete(id, QList<QByteArray>());
            return;
        }
        locations = Rdm::readValue<QSet<RTags::Location> >(db.db(), symbolName.constData());
        if (locations.isEmpty()) {
            emit complete(id, QList<QByteArray>());
            return;
        }
        db.close();
    }
    if (!db.open(Database::Symbol, LevelDB::ReadOnly)) {
        emit complete(id, QList<QByteArray>());
        return;
    }
    QList<QByteArray> list;
    foreach(const RTags::Location &location, locations) {
        Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db.db(), location);
        QSet<RTags::Location> refs = cursorInfo.references;
        if (refs.isEmpty() && !cursorInfo.target.isNull()) {
            cursorInfo = Rdm::findCursorInfo(db.db(), cursorInfo.target);
            refs = cursorInfo.references;
        }
        foreach (const RTags::Location &loc, cursorInfo.references) {
            list.append(loc.key(includeContext ? RTags::Location::ShowContext : RTags::Location::NoFlag));
        }
    }
    emit complete(id, list);
}
