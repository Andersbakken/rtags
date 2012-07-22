#include "Database.h"
#include "ReferencesJob.h"
#include "Server.h"
#include "RTags.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(const Location &loc, const QueryMessage &query)
    : Job(query, 0)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(const ByteArray &sym, const QueryMessage &query)
    : Job(query, 0), symbolName(sym)
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
    const unsigned keyFlags = Job::keyFlags();
    const unsigned flags = queryFlags();
    Set<Location> refs, additionalReferences;
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        // error() << "looking up refs for " << it->key() << bool(flags & QueryMessage::AllReferences);
        if (isAborted())
            return;

        process(db, *it, refs, flags & QueryMessage::ReferencesForRenameSymbol ? &additionalReferences : 0);
        // for (Set<Location>::const_iterator b = refs.begin(); b != refs.end(); ++b) {
        //     error() << "found " << b->key();
        // }

    }
    error() << "got additionalReferences" << additionalReferences;
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
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    Location realLoc;
    CursorInfo cursorInfo = RTags::findCursorInfo(db, location, &realLoc);
    // error() << location.key() << cursorInfo.kind;
    if (RTags::isReference(cursorInfo.kind)) {
        realLoc = cursorInfo.target;
        cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
    }
    // error() << "refs for " << location.key() << allReferences
    //         << cursorInfo << realLoc.key() << "isReference" << RTags::isReference(cursorInfo.kind);

    if (cursorInfo.isValid()) {
        const bool noReferences = (allReferences
                                   && (cursorInfo.kind == CXCursor_Constructor || cursorInfo.kind == CXCursor_Destructor)
                                   && additionalReferences);
        if (additionalReferences)
            *additionalReferences += cursorInfo.additionalReferences;
        // error() << noReferences << location.key();
        if (!noReferences)
            refs += cursorInfo.references;
        if (cursorInfo.target.isValid() && cursorInfo.kind != CXCursor_VarDecl) {
            if (allReferences) {
                refs.insert(cursorInfo.target);
            }
            cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
            if (!noReferences)
                refs += cursorInfo.references;
            if (additionalReferences)
                *additionalReferences += cursorInfo.additionalReferences;
        }
        // error() << "here" << cursorInfo.symbolName << "noReferences" << noReferences << "allReferences" << allReferences << realLoc;
        if (!noReferences && allReferences) {
            refs.insert(realLoc);
        }
    }

}
