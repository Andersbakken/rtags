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
        ScopedDB db = Server::instance()->db(Server::SymbolName, Server::Read);
        locations = db->value<Set<Location> >(symbolName);
        if (locations.isEmpty()) {
            return;
        }
    }
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    ScopedDB db = Server::instance()->db(Server::Symbol, Server::Read);
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
            for (Set<Location>::const_iterator rit = references.begin(); rit != references.end(); ++rit) {
                const CursorInfo container = RTags::findCursorInfo(db, *rit);
                if (container.kind == CXCursor_ClassDecl || container.kind == CXCursor_StructDecl) {
                    process(db, *rit, container);
                    break;
                }
            }
            continue;
        }
        process(db, pos, cursorInfo);
        if (cursorInfo.target.isValid()) {
            const CursorInfo target = RTags::findCursorInfo(db, cursorInfo.target);
            if (target.kind == cursorInfo.kind) {
                process(db, cursorInfo.target, target);
            }
        }

        for (Set<Location>::const_iterator ait = additional.begin(); ait != additional.end(); ++ait) {
            cursorInfo = RTags::findCursorInfo(db, *ait);
            process(db, *ait, cursorInfo);
            if (cursorInfo.target.isValid()) {
                const CursorInfo target = RTags::findCursorInfo(db, cursorInfo.target);
                if (target.kind == cursorInfo.kind) {
                    process(db, cursorInfo.target, target);
                }
            }
        }
        additional.clear();
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
    bool classOrStruct = false;
    bool constructorOrDestructor = false;
    bool memberFunction = false;
    switch (cursorInfo.kind) {
    case CXCursor_CXXMethod:
        memberFunction = true;
        break;
    case CXCursor_StructDecl:
    case CXCursor_ClassDecl:
        classOrStruct = true;
        break;
    case CXCursor_Constructor:
    case CXCursor_Destructor:
        assert(!allReferences);
        constructorOrDestructor = true;
        break;
    default:
        break;
    }

    // error() << pos << cursorInfo << "allReferences" << allReferences;
    assert(!RTags::isReference(cursorInfo.kind));
    if (allReferences)
        references.insert(pos);
    if (memberFunction) {
        for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
            const CursorInfo ci = RTags::findCursorInfo(db, *it);
            if (RTags::isReference(ci.kind)) {
                references.insert(*it);
            } else {
                additional.insert(*it);
            }
        }
    } else if (!allReferences && (classOrStruct || constructorOrDestructor)) {
        for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
            const CursorInfo ci = RTags::findCursorInfo(db, *it);
            if (RTags::isReference(ci.kind))
                references.insert(*it);
        }
    } else {
        references += cursorInfo.references;
    }
}
