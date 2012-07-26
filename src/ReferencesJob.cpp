#include "Database.h"
#include "ReferencesJob.h"
#include "Server.h"
#include "RTags.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(const Location &loc, const QueryMessage &query)
    : Job(query, 0), renamingClass(false)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(const ByteArray &sym, const QueryMessage &query)
    : Job(query, 0), symbolName(sym), renamingClass(false)
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
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        error() << "looking up refs for " << it->key() << bool(flags & QueryMessage::ReferencesForRenameSymbol);
        if (isAborted())
            return;

        Location pos;
        CursorInfo cursorInfo = RTags::findCursorInfo(db, *it, &pos);
        if (RTags::isReference(cursorInfo.kind)) {
            pos = cursorInfo.target;
            cursorInfo = RTags::findCursorInfo(db, cursorInfo.target);
        }
        if (queryFlags() & QueryMessage::ReferencesForRenameSymbol
            && (cursorInfo.kind == CXCursor_Constructor || cursorInfo.kind == CXCursor_Destructor)) {
            if (!cursorInfo.additionalReferences.empty()) {
                const Location &loc = *cursorInfo.additionalReferences.begin();
                const CursorInfo container = RTags::findCursorInfo(db, loc);
                if (container.isValid()) {
                    assert(container.kind == CXCursor_ClassDecl || container.kind == CXCursor_StructDecl);
                    process(loc, container);
                }
            }
            continue;
        }
        process(pos, cursorInfo);
        if (cursorInfo.target.isValid()) {
            const CursorInfo target = RTags::findCursorInfo(db, cursorInfo.target);
            if (target.kind == cursorInfo.kind)
                process(cursorInfo.target, target);
        }
    }

    List<Location> sorted = references.toList();
    if (flags & QueryMessage::ReverseSort) {
        std::sort(sorted.begin(), sorted.end(), std::greater<Location>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    for (List<Location>::const_iterator it = sorted.begin(); it != sorted.end(); ++it) {
        Location l = *it;
        if (renamingClass) {
            CursorInfo ci = RTags::findCursorInfo(db, l);
            if (ci.kind == CXCursor_Destructor) {
                l = Location(l.fileId(), l.offset() + 1);
            }
        }

        write(l.key(keyFlags));
    }
}

void ReferencesJob::process(const Location &pos, const CursorInfo &cursorInfo)
{
    if (!cursorInfo.isValid())
        return;
    if (cursorInfo.kind == CXCursor_StructDecl || cursorInfo.kind == CXCursor_ClassDecl)
        renamingClass = true;
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    error() << pos << cursorInfo << "allReferences" << allReferences;
    assert(!RTags::isReference(cursorInfo.kind));
    if (allReferences) {
        references.insert(pos);
        references += cursorInfo.references + cursorInfo.additionalReferences;
    } else {
        references += cursorInfo.references;
        if (cursorInfo.kind == CXCursor_CXXMethod)
            references += cursorInfo.additionalReferences;
    }
}
