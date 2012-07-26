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
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Read);
    const unsigned keyFlags = Job::keyFlags();
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        // error() << "looking up refs for " << it->key() << bool(flags & QueryMessage::ReferencesForRenameSymbol);
        if (isAborted())
            return;

        Location pos;
        CursorInfo cursorInfo = RTags::findCursorInfo(db, *it, &pos);
        if (RTags::isReference(cursorInfo.kind)) {
            pos = cursorInfo.target;
            cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
        }
        if (allReferences && (cursorInfo.kind == CXCursor_Constructor || cursorInfo.kind == CXCursor_Destructor)) {
            if (!cursorInfo.additionalReferences.empty()) {
                const Location &loc = *cursorInfo.additionalReferences.begin();
                const CursorInfo container = RTags::findCursorInfo(db, loc);
                if (container.isValid()) {
                    assert(container.kind == CXCursor_ClassDecl || container.kind == CXCursor_StructDecl);
                    process(db, loc, container);
                }
            }
            continue;
        }
        Set<Location> additionalReferences;
        process(db, pos, cursorInfo);
        if (cursorInfo.kind == CXCursor_CXXMethod)
            additionalReferences += cursorInfo.additionalReferences;
        if (cursorInfo.target.isValid()) {
            const CursorInfo target = RTags::findCursorInfo(db, cursorInfo.target);
            if (target.kind == cursorInfo.kind) {
                process(db, cursorInfo.target, target);
                if (target.kind == CXCursor_CXXMethod)
                    additionalReferences += target.additionalReferences;
            }
        }
        for (Set<Location>::const_iterator ait = additionalReferences.begin(); ait != additionalReferences.end(); ++ait) {
            process(db, *ait, RTags::findCursorInfo(db, *ait));
        }
    }

    List<Location> sorted = references.toList();
    if (queryFlags() & QueryMessage::ReverseSort) {
        std::sort(sorted.begin(), sorted.end(), std::greater<Location>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    for (List<Location>::const_iterator it = sorted.begin(); it != sorted.end(); ++it) {
        write(it->key(keyFlags));
    }
}

void ReferencesJob::process(ScopedDB &db, const Location &pos, const CursorInfo &cursorInfo)
{
    if (!cursorInfo.isValid())
        return;
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    const bool classOrStruct = (cursorInfo.kind == CXCursor_StructDecl || cursorInfo.kind == CXCursor_ClassDecl);
    // error() << pos << cursorInfo << "allReferences" << allReferences;
    assert(!RTags::isReference(cursorInfo.kind));
    if (allReferences) {
        references.insert(pos);
        references += cursorInfo.references;
        if (classOrStruct) {
            for (Set<Location>::const_iterator it = cursorInfo.additionalReferences.begin(); it != cursorInfo.additionalReferences.end(); ++it) {
                CursorInfo ci = RTags::findCursorInfo(db, *it);
                if (ci.kind == CXCursor_Destructor) {
                    references.insert(Location(it->fileId(), it->offset() + 1));
                } else {
                    references.insert(*it);
                }
            }
        }
    } else {
        if (classOrStruct) {
            for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
                const CursorInfo ci = RTags::findCursorInfo(db, *it);
                // This is some awful stuff right here. We have references to
                // the class that should be to the constructor, we need them for
                // renaming but not for normal references so we do this.
                if (ci.target == pos) {
                    references.insert(*it);
                }
            }
        } else {
            references += cursorInfo.references;
        }
    }
}
