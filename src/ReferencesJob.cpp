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
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Read);
    const unsigned keyFlags = QueryMessage::keyFlags(flags);
    Set<Location> refs, additionalReferences;
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        // error() << "looking up refs for " << it->key() << " " << bool(flags & QueryMessage::AllReferences);
        if (isAborted())
            return;

        process(db, *it, refs, flags & QueryMessage::AllReferences ? &additionalReferences : 0);
        // for (Set<Location>::const_iterator b = refs.begin(); b != refs.end(); ++b) {
        //     error() << "found " << b->key();
        // }

    }
    for (Set<Location>::const_iterator it = additionalReferences.begin(); it != additionalReferences.end(); ++it) {
        if (isAborted())
            return;

        process(db, *it, refs, 0);
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

void ReferencesJob::process(ScopedDB &db, const Location &location, Set<Location> &refs, Set<Location> *additionalReferences)
{
    const bool allReferences = flags & QueryMessage::AllReferences;
    Location realLoc;
    CursorInfo cursorInfo = RTags::findCursorInfo(db, location, &realLoc);
    if (RTags::isReference(cursorInfo.kind)) {
        realLoc = cursorInfo.target;
        cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
    }

    // error() << "refs for " << location.key() << " " << allReferences
    //         << " " << cursorInfo.isValid();

    if (cursorInfo.isValid()) {
        const bool wantsReferences = (!allReferences
                                      || cursorInfo.kind == CXCursor_StructDecl
                                      || cursorInfo.kind == CXCursor_ClassDecl);
        if (additionalReferences)
            *additionalReferences += cursorInfo.additionalReferences;
        if (wantsReferences)
            refs += cursorInfo.references;
        if (cursorInfo.target.isValid() && cursorInfo.kind != CXCursor_VarDecl) {
            if (wantsReferences)
                refs += cursorInfo.references;
            if (allReferences) {
                refs.insert(cursorInfo.target);
            }
            cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
            if (additionalReferences)
                *additionalReferences += cursorInfo.additionalReferences;
        }
        if (allReferences) {
            refs.insert(realLoc);
        }
    }

}
