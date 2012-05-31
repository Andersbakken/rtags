#include "Database.h"
#include "ReferencesJob.h"
#include "Server.h"
#include "Rdm.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(int i, const Location &loc, unsigned fl)
    : Job(i), symbolName(QByteArray()), flags(fl)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(int i, const QByteArray &sym, unsigned fl)
    : Job(i), symbolName(sym), flags(fl)
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
    const bool excludeDefsAndDecls = !(flags & QueryMessage::IncludeDeclarationsAndDefinitions);
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    const unsigned keyFlags = QueryMessage::keyFlags(flags);
    foreach(const Location &location, locations) {
        if (isAborted())
            return;
        QSet<Location> refs;
        Location loc = location;
        QSet<Location> filtered;

        CursorInfo cursorInfo = Rdm::findCursorInfo(db, loc);
        if (clang_isReference(cursorInfo.kind)) {
            filtered.insert(cursorInfo.target);
            cursorInfo = Rdm::findCursorInfo(db, cursorInfo.target);
        } else {
            filtered.insert(location);
        }
        if (cursorInfo.isValid()) {
            if (excludeDefsAndDecls && cursorInfo.target.isValid())
                filtered.insert(cursorInfo.target);
            assert(!clang_isReference(cursorInfo));
            foreach(const Location &l, cursorInfo.references) {
                if (!excludeDefsAndDecls || !filtered.contains(l)) {
                    refs.insert(l);
                }
            }
            assert(filtered.target != cursorInfo.location);
            if (cursorInfo.target.isValid()) {
                cursorInfo = Rdm::findCursorInfo(db, cursorInfo.target);
                foreach(const Location &l, cursorInfo.references) {
                    if (!excludeDefsAndDecls || !filtered.contains(l)) {
                        refs.insert(l);
                    }
                }
            }
        }
        QList<Location> sorted = refs.toList();
        qSort(sorted);
        foreach (const Location &loc, sorted) {
            write(loc.key(keyFlags));
        }
    }
}
