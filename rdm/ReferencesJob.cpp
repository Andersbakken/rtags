#include "ReferencesJob.h"
#include "Server.h"
#include "LevelDB.h"
#include "Rdm.h"

ReferencesJob::ReferencesJob(int i, const RTags::Location &loc, unsigned flags)
    : Job(i), symbolName(QByteArray()), keyFlags(flags)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(int i, const QByteArray &sym, unsigned flags)
    : Job(i), symbolName(sym), keyFlags(flags)
{
}

void ReferencesJob::run()
{
    LevelDB db;
    if (!symbolName.isEmpty()) {
        if (!db.open(Server::SymbolName, LevelDB::ReadOnly)) {
            emit complete(id());
            return;
        }
        locations = Rdm::readValue<QSet<RTags::Location> >(db.db(), symbolName.constData());
        if (locations.isEmpty()) {
            emit complete(id());
            return;
        }
        db.close();
    }
    if (!db.open(Server::Symbol, LevelDB::ReadOnly)) {
        emit complete(id());
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
        foreach (const RTags::Location &loc, refs) {
            list.append(loc.key(keyFlags));
        }
    }
    emit complete(id(), list);
}
