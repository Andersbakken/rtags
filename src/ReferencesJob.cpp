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

void ReferencesJob::run()
{
    shared_ptr<Project> proj = project();
    const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
    Location startLocation;
    if (proj->indexer) {
        if (!symbolName.isEmpty()) {
            Scope<const SymbolNameMap&> scope = proj->lockSymbolNamesForRead();
            locations = scope.data().value(symbolName);
        }
        if (!locations.isEmpty()) {
            Scope<const SymbolMap&> scope = proj->lockSymbolsForRead();
            const SymbolMap &map = scope.data();
            for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
                // error() << "looking up refs for " << it->key() << bool(flags & QueryMessage::ReferencesForRenameSymbol);
                Location pos;
                CursorInfo cursorInfo = RTags::findCursorInfo(map, *it, &pos);
                startLocation = pos;
                if (RTags::isReference(cursorInfo.kind)) {
                    pos = cursorInfo.target;
                    cursorInfo = RTags::findCursorInfo(map, cursorInfo.target, 0);
                }
                if (allReferences && (cursorInfo.kind == CXCursor_Constructor || cursorInfo.kind == CXCursor_Destructor)) {
                    // In this case we have additional references that are the
                    // actual class and structs that we want to include. Also,
                    // we don't want to include actual calls to the constructor
                    // or destructor for renaming purposes.
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

    if (!symbolName.isEmpty() && !(queryFlags() & QueryMessage::DisableGRTags) && proj->grtags) {
        Scope<const GRMap&> scope = proj->lockGRForRead();
        const GRMap &map = scope.data();
        const GRMap::const_iterator it = map.find(symbolName);
        if (it != map.end()) {
            const Map<Location, bool> &values = it->second;
            for (Map<Location, bool>::const_iterator it = values.begin(); it != values.end(); ++it) {
                if (allReferences || it->second)
                    references.insert(it->first);
            }
        }
    }

    List<Location> sorted = references.toList();
    if (queryFlags() & QueryMessage::ReverseSort) {
        std::sort(sorted.begin(), sorted.end(), std::greater<Location>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    // We don't want to do the startIndex stuff when renaming. The only way to
    // tell the difference between rtags-find-all-references and
    // rtags-rename-symbol is that the latter does a reverse sort. It kinda
    // doesn't make sense to have this behavior in reverse sort anyway so I
    // won't formalize the rename parameters to indicate that we're renaming
    int startIndex = 0;
    const int count = sorted.size();
    if (!(queryFlags() && QueryMessage::ReverseSort) && sorted.size() != 1 && !startLocation.isNull()) {
        startIndex = sorted.indexOf(startLocation) + 1;
    }
    const unsigned keyFlags = Job::keyFlags();

    for (int i=0; i<count; ++i) {
        const Location &loc = sorted.at((startIndex + i) % count);
        write(loc.key(keyFlags));
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
            // since member functions can have other reimplementations of the
            // same virtual function in their references we have to make sure
            // it's an actual reference before adding it.
            if (RTags::isReference(ci.kind)) {
                references.insert(*it);
            } else {
                additional.insert(*it);
            }
        }
    } else if (!allReferences && (classOrStruct || constructorOrDestructor)) {
        for (Set<Location>::const_iterator it = cursorInfo.references.begin(); it != cursorInfo.references.end(); ++it) {
            const CursorInfo ci = RTags::findCursorInfo(map, *it, 0);
            // since classes, structs, constructors and destructors have
            // additional stuff in their references we have ot have sure it's an
            // actual reference before adding it. For constructors we also have
            // to include CXCursor_VarDecl since they're a cursor in their own
            // right in addition to be being references to a constructor.
            //
            // In the case of constructor initalizer lists the actual type is
            // CXCursor_MemberRef so it returns true for RTags::isReference.
            if (RTags::isReference(ci.kind) || (cursorInfo.kind == CXCursor_Constructor && ci.kind == CXCursor_VarDecl)) {
                references.insert(*it);
            }
        }
    } else {
        references += cursorInfo.references;
    }
}
