#include "Database.h"
#include "ReferencesJob.h"
#include "Server.h"
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
        ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Read);
        locations = db->value<QSet<Location> >(symbolName);
        if (locations.isEmpty()) {
            return;
        }
    }
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    foreach(const Location &location, locations) {
        if (isAborted())
            return;
        QSet<Location> refs;
        Location loc = location;
        QSet<Location> seen;
        /* This loop goes three times because we might get called for one of the
         * actual references. If so we want to follow the location to the
         * declaration or definition (whichever one the reference actually
         * references... it can be either). From there we want to get those
         * references and follow to the other one (decl => def or def => decl)
         * and get those references. Wes would be proud! */

        for (int i=0; i<3; ++i) {
            CursorInfo cursorInfo = Rdm::findCursorInfo(db, loc);
            refs += cursorInfo.references;
            loc = cursorInfo.target;
            if (loc.isNull() || seen.contains(loc))
                break;
            seen.insert(loc);
        }
        QList<Location> sorted = refs.toList();
        qSort(sorted);
        foreach (const Location &loc, sorted) {
            write(loc.key(keyFlags));
        }
    }
}
