#include "Database.h"
#include "ReferencesJob.h"
#include "Server.h"
#include "RTags.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(int i, const Location &loc, unsigned fl)
    : Job(i, QueryJobPriority), symbolName(ByteArray()), flags(fl)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(int i, const ByteArray &sym, unsigned fl)
    : Job(i, QueryJobPriority), symbolName(sym), flags(fl)
{
}

void ReferencesJob::execute()
{
    if (!symbolName.isEmpty()) {
        ScopedDB db = Server::instance()->db(Server::SymbolName, ReadWriteLock::Read);
        locations = db->value<Set<Location> >(symbolName);
        if (locations.isEmpty()) {
            return;
        }
    }
    const bool excludeDefsAndDecls = !(flags & QueryMessage::IncludeDeclarationsAndDefinitions);
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Read);
    const unsigned keyFlags = QueryMessage::keyFlags(flags);
    Set<Location> refs;
    Set<Location> filtered;
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        if (isAborted())
            return;

        const Location &location = *it;
        Location realLoc;
        CursorInfo cursorInfo = RTags::findCursorInfo(db, location, &realLoc);
        if (RTags::isReference(cursorInfo.kind)) {
            if (excludeDefsAndDecls) {
                filtered.insert(cursorInfo.target);
            } else {
                refs.insert(cursorInfo.target);
            }
            cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
        } else {
            if (excludeDefsAndDecls) {
                filtered.insert(realLoc);
            } else {
                refs.insert(realLoc);
            }
        }

        if (cursorInfo.symbolLength) {
            if (cursorInfo.target.isValid()) {
                if (excludeDefsAndDecls) {
                    filtered.insert(cursorInfo.target);
                } else {
                    refs.insert(cursorInfo.target);
                }
            }
            for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
                const Location &l = *it;
                if (!excludeDefsAndDecls || !filtered.contains(l)) {
                    refs.insert(l);
                }
            }
            if (cursorInfo.target.isValid() && cursorInfo.kind != CXCursor_VarDecl) {
                cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
                for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
                    const Location &l = *it;
                    if (!excludeDefsAndDecls || !filtered.contains(l)) {
                        refs.insert(l);
                    }
                }
            }
        }
    }
    List<Location> sorted = refs.toList();
    if (flags & QueryMessage::ReverseSort) {
        std::sort(sorted.begin(), sorted.end(), std::greater<Location>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    for (List<Location>::const_iterator it = sorted.begin(); it != sorted.end(); ++it) {
        const Location &l = *it;
        write(l.key(keyFlags));
    }
}
