#include "ReferencesJob.h"
#include "Server.h"
#include "RTags.h"
#include "CursorInfo.h"
#include "Project.h"

ReferencesJob::ReferencesJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, 0, proj)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(const String &sym, const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, 0, proj), symbolName(sym)
{
}

void ReferencesJob::execute()
{
    shared_ptr<Project> proj = project();
    Location startLocation;
    Map<Location, std::pair<bool, CXCursorKind> > references;
    if (proj) {
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
                bool moved = false;
                CursorInfo cursorInfo = RTags::findCursorInfo(map, *it,
                                                              symbolName.isEmpty() && queryFlags() & QueryMessage::ValidateSymbol
                                                              ? &moved : 0, &pos);
                if (moved) {
                    write("Symbol has moved");
                    return;
                }
                if (startLocation.isNull())
                    startLocation = pos;
                if (RTags::isReference(cursorInfo.kind)) {
                    cursorInfo = cursorInfo.bestTarget(map, &pos);
                }
                if (queryFlags() & QueryMessage::AllReferences) {
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
                            references[a->first] = std::make_pair(a->second.isDefinition(), a->second.kind);
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
                                references[a->first] = std::make_pair(a->second.isDefinition(), a->second.kind);
                            }
                        }
                    }
                } else if (queryFlags() & QueryMessage::FindVirtuals) {
                    // ### not supporting DeclarationOnly
                    const SymbolMap virtuals = cursorInfo.virtuals(pos, map);
                    List<RTags::SortedCursor> sortedCursors;
                    sortedCursors.reserve(virtuals.size());
                    for (SymbolMap::const_iterator v = virtuals.begin(); v != virtuals.end(); ++v) {
                        references[v->first] = std::make_pair(v->second.isDefinition(), v->second.kind);
                    }
                } else {
                    const SymbolMap callers = cursorInfo.callers(pos, map);
                    for (SymbolMap::const_iterator c = callers.begin(); c != callers.end(); ++c) {
                        references[c->first] = std::make_pair(false, CXCursor_FirstInvalid);
                        // For find callers we don't want to prefer definitions or do ranks on cursors
                    }
                }
            }
        }
    }
    enum { Rename = (QueryMessage::ReverseSort|QueryMessage::AllReferences) };
    if ((queryFlags() & Rename) == Rename) {
        if (!references.isEmpty()) {
            Map<Location, std::pair<bool, CXCursorKind> >::const_iterator it = references.end();
            do {
                --it;
                write(it->first);
            } while (it != references.begin());
        }
    } else {
        List<RTags::SortedCursor> sorted;
        sorted.reserve(references.size());
        for (Map<Location, std::pair<bool, CXCursorKind> >::const_iterator it = references.begin();
             it != references.end(); ++it) {
            sorted.append(RTags::SortedCursor(it->first, it->second.first, it->second.second));
        }
        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedCursor>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        int startIndex = 0;
        const int count = sorted.size();
        if (!startLocation.isNull()) {
            for (int i=0; i<count; ++i) {
                if (sorted.at(i).location == startLocation) {
                    startIndex = i + 1;
                    break;
                }
            }
        }

        for (int i=0; i<count; ++i) {
            const Location &loc = sorted.at((startIndex + i) % count).location;
            write(loc);
        }
    }
}
