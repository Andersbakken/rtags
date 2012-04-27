#include "ReferencesJob.h"
#include "Server.h"
#include "leveldb/db.h"
#include "Rdm.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(int i, const Location &loc, unsigned flags)
    : Job(i), symbolName(QByteArray()), keyFlags(flags)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(int i, const QByteArray &sym, unsigned flags)
    : Job(i), symbolName(sym), keyFlags(flags)
{
}

void ReferencesJob::execute()
{
    if (!symbolName.isEmpty()) {
        leveldb::DB *db = Server::instance()->db(Server::SymbolName);
        locations = Rdm::readValue<QSet<Location> >(db, symbolName.constData());
        if (locations.isEmpty()) {
            return;
        }
    }
    leveldb::DB *db = Server::instance()->db(Server::Symbol);
    foreach(const Location &location, locations) {
        if (isAborted())
            return;
        CursorInfo cursorInfo = Rdm::findCursorInfo(db, location);
        QSet<Location> refs = cursorInfo.references;
        if (refs.isEmpty() && !cursorInfo.target.isNull()) {
            cursorInfo = Rdm::findCursorInfo(db, cursorInfo.target);
            refs = cursorInfo.references;
        }
        foreach (const Location &loc, refs) {
            write(loc.key(keyFlags));
        }
    }
}
