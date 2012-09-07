#include "ReferencesJob.h"
#include "Server.h"
#include "RTags.h"
#include "CursorInfo.h"

ReferencesJob::ReferencesJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, 0, proj)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(const ByteArray &sym, const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, 0, proj), symbolName(sym)
{
}

void ReferencesJob::execute()
{
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    if (project()->indexer) {
        if (!symbolName.isEmpty()) {
            Scope<const SymbolNameMap&> scope = project()->lockSymbolNamesForRead();
            locations = scope.data().value(symbolName);
        }
        if (!locations.isEmpty()) {
            Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
            const SymbolMap &map = scope.data();
            for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
                // error() << "looking up refs for " << it->key() << bool(flags & QueryMessage::ReferencesForRenameSymbol);
                Location pos;
                CursorInfo cursorInfo = RTags::findCursorInfo(map, *it, &pos);
                if (RTags::isReference(cursorInfo.kind)) {
                    pos = cursorInfo.target;
                    cursorInfo = RTags::findCursorInfo(map, cursorInfo.target, 0);
                }
                if (allReferences && (cursorInfo.kind == CXCursor_Constructor || cursorInfo.kind == CXCursor_Destructor)) {
                    for (Set<Location>::const_iterator rit = references.begin(); rit != references.end(); ++rit) {
                        const CursorInfo container = RTags::findCursorInfo(map, *rit, 0);
                        if (container.kind == CXCursor_ClassDecl || container.kind == CXCursor_StructDecl) {
                            process(map, *rit, container);
                            break;
                        }
                    }
                    continue;
                }
                process(map, pos, cursorInfo);
                if (cursorInfo.target.isValid()) {
                    const CursorInfo target = RTags::findCursorInfo(map, cursorInfo.target, 0);
                    if (target.kind == cursorInfo.kind) {
                        process(map, cursorInfo.target, target);
                    }
                }

                for (Set<Location>::const_iterator ait = additional.begin(); ait != additional.end(); ++ait) {
                    cursorInfo = RTags::findCursorInfo(map, *ait, 0);
                    process(map, *ait, cursorInfo);
                    if (cursorInfo.target.isValid()) {
                        const CursorInfo target = RTags::findCursorInfo(map, cursorInfo.target, 0);
                        if (target.kind == cursorInfo.kind) {
                            process(map, cursorInfo.target, target);
                        }
                    }
                }
                additional.clear();
            }
        }
    }

    // if (!symbolName.isEmpty() && !(queryFlags() & QueryMessage::DisableGRTags) && (project()->grtags->flags() & GRTags::Parse)) {
    //     ScopedDB database = db(Project::GR, ReadWriteLock::Read);
    //     const Map<Location, bool> values = database->value<Map<Location, bool> >(symbolName);
    //     database.reset();
    //     for (Map<Location, bool>::const_iterator it = values.begin(); it != values.end(); ++it) {
    //         if (allReferences || it->second)
    //             references.insert(it->first);
    //     }
    // }

    List<Location> sorted = references.toList();
    if (queryFlags() & QueryMessage::ReverseSort) {
        std::sort(sorted.begin(), sorted.end(), std::greater<Location>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    const unsigned keyFlags = Job::keyFlags();
    for (List<Location>::const_iterator it = sorted.begin(); it != sorted.end(); ++it) {
        write(it->key(keyFlags));
    }
}

void ReferencesJob::process(const SymbolMap &map, const Location &pos, const CursorInfo &cursorInfo)
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
            const CursorInfo ci = RTags::findCursorInfo(map, *it, 0);
            if (RTags::isReference(ci.kind)) {
                references.insert(*it);
            } else {
                additional.insert(*it);
            }
        }
    } else if (!allReferences && (classOrStruct || constructorOrDestructor)) {
        for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
            const CursorInfo ci = RTags::findCursorInfo(map, *it, 0);
            if (RTags::isReference(ci.kind))
                references.insert(*it);
        }
    } else {
        references += cursorInfo.references;
    }
}
