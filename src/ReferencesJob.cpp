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
    Location startLocation;
    Set<Location> references;
    if (proj->indexer) {
        if (!symbolName.isEmpty()) {
            Scope<const SymbolNameMap&> scope = proj->lockSymbolNamesForRead();
            if (scope.isNull())
                return;
            locations = scope.data().value(symbolName);
        }
        if (!locations.isEmpty()) {
            Scope<const SymbolMap&> scope = proj->lockSymbolsForRead();
            if (scope.isNull())
                return;

            const SymbolMap &map = scope.data();
            for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
                Location pos;
                CursorInfo cursorInfo = RTags::findCursorInfo(map, *it, &pos);
                if (startLocation.isNull())
                    startLocation = pos;
                if (RTags::isReference(cursorInfo.kind)) {
                    cursorInfo = cursorInfo.bestTarget(map, &pos);
                }
                if (queryFlags() & QueryMessage::ReferencesForRenameSymbol) {
                    const SymbolMap all = cursorInfo.allReferences(pos, map);
                    bool classRename = false;
                    switch (cursorInfo.kind) {
                    case CXCursor_Constructor:
                    case CXCursor_Destructor:
                        classRename = true;
                        break;
                    default:
                        classRename = cursorInfo.isClass();
                        break;
                    }

                    for (SymbolMap::const_iterator a = all.begin(); a != all.end(); ++a) {
                        if (!classRename) {
                            references.insert(a->first);
                        } else {
                            enum State {
                                FoundConstructor = 0x1,
                                FoundClass = 0x2,
                                FoundReferences = 0x4
                            };
                            unsigned state = 0;
                            const SymbolMap targets = a->second.targetInfos(map);
                            for (SymbolMap::const_iterator t = targets.begin(); t != targets.end(); ++t) {
                                if (t->second.kind != a->second.kind)
                                    state |= FoundReferences;
                                if (t->second.kind == CXCursor_Constructor) {
                                    state |= FoundConstructor;
                                } else if (t->second.isClass()) {
                                    state |= FoundClass;
                                }
                            }
                            if ((state & (FoundConstructor|FoundClass)) != FoundConstructor || !(state & FoundReferences)) {
                                references.insert(a->first);
                            }
                        }
                    }
                } else if (queryFlags() & QueryMessage::FindVirtuals) {
                    const SymbolMap virtuals = cursorInfo.virtuals(pos, map);
                    for (SymbolMap::const_iterator v = virtuals.begin(); v != virtuals.end(); ++v) {
                        references.insert(v->first);
                    }
                } else {
                    const SymbolMap callers = cursorInfo.callers(pos, map);
                    for (SymbolMap::const_iterator c = callers.begin(); c != callers.end(); ++c) {
                        references.insert(c->first);
                    }
                }
            }
        }
    }

    if (!symbolName.isEmpty() && proj->grtags) {
        Scope<const GRMap&> scope = proj->lockGRForRead();
        const GRMap &map = scope.data();
        const GRMap::const_iterator it = map.find(symbolName);
        if (it != map.end()) {
            const Map<Location, bool> &values = it->second;
            const bool allReferences = queryFlags() & QueryMessage::ReferencesForRenameSymbol;
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
    if (!(queryFlags() & QueryMessage::ReverseSort) && sorted.size() != 1 && !startLocation.isNull()) {
        startIndex = sorted.indexOf(startLocation) + 1;
    }
    const unsigned keyFlags = Job::keyFlags();

    for (int i=0; i<count; ++i) {
        const Location &loc = sorted.at((startIndex + i) % count);
        write(loc.key(keyFlags));
    }
}
