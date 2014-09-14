/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "ReferencesJob.h"
#include "Server.h"
#include "RTags.h"
#include "CursorInfo.h"
#include "Project.h"

ReferencesJob::ReferencesJob(const Location &loc, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, 0, proj)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(const String &sym, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, 0, proj), symbolName(sym)
{
}

int ReferencesJob::execute()
{
    std::shared_ptr<Project> proj = project();
    Location startLocation;
    Map<Location, std::pair<bool, uint16_t> > references;
    if (proj) {
        if (!symbolName.isEmpty())
            locations = proj->locations(symbolName);
        if (!locations.isEmpty()) {
            const SymbolMap &map = proj->symbols();

            for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
                Location pos;
                SymbolMap::const_iterator found;
                found = RTags::findCursorInfo(map, *it);
                if (found == map.end())
                    continue;
                pos = found->first;
                if (startLocation.isNull())
                    startLocation = pos;
                std::shared_ptr<CursorInfo> cursorInfo = found->second;
                if (!cursorInfo)
                    continue;
                if (RTags::isReference(cursorInfo->kind)) {
                    cursorInfo = cursorInfo->bestTarget(map, &pos);
                    if (!cursorInfo)
                        continue;
                }
                if (queryFlags() & QueryMessage::AllReferences) {
                    const SymbolMap all = cursorInfo->allReferences(pos, map);

                    bool classRename = false;
                    switch (cursorInfo->kind) {
                    case CXCursor_Constructor:
                    case CXCursor_Destructor:
                        classRename = true;
                        break;
                    default:
                        classRename = cursorInfo->isClass();
                        break;
                    }

                    for (SymbolMap::const_iterator a = all.begin(); a != all.end(); ++a) {
                        if (!classRename) {
                            references[a->first] = std::make_pair(a->second->isDefinition(), a->second->kind);
                        } else {
                            enum State {
                                FoundConstructor = 0x1,
                                FoundClass = 0x2,
                                FoundReferences = 0x4
                            };
                            unsigned state = 0;
                            const SymbolMap targets = a->second->targetInfos(map);
                            for (SymbolMap::const_iterator t = targets.begin(); t != targets.end(); ++t) {
                                if (t->second->kind != a->second->kind)
                                    state |= FoundReferences;
                                if (t->second->kind == CXCursor_Constructor) {
                                    state |= FoundConstructor;
                                } else if (t->second->isClass()) {
                                    state |= FoundClass;
                                }
                            }
                            if ((state & (FoundConstructor|FoundClass)) != FoundConstructor || !(state & FoundReferences)) {
                                references[a->first] = std::make_pair(a->second->isDefinition(), a->second->kind);
                            }
                        }
                    }
                } else if (queryFlags() & QueryMessage::FindVirtuals) {
                    const SymbolMap virtuals = cursorInfo->virtuals(pos, map);
                    const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
                    for (SymbolMap::const_iterator v = virtuals.begin(); v != virtuals.end(); ++v) {
                        const bool def = v->second->isDefinition();
                        if (declarationOnly && def) {
                            const std::shared_ptr<CursorInfo> decl = v->second->bestTarget(map);
                            if (decl && !decl->isNull())
                                continue;
                        }
                        references[v->first] = std::make_pair(def, v->second->kind);
                    }
                    startLocation.clear();
                    // since one normally calls this on a declaration it kinda
                    // doesn't work that well do the clever offset thing
                    // underneath
                } else {
                    const SymbolMap callers = cursorInfo->callers(pos, map);
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
            Map<Location, std::pair<bool, uint16_t> >::const_iterator it = references.end();
            do {
                --it;
                write(it->first);
            } while (it != references.begin());
            return 0;
        }
    } else {
        List<RTags::SortedCursor> sorted;
        sorted.reserve(references.size());
        for (Map<Location, std::pair<bool, uint16_t> >::const_iterator it = references.begin();
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
        if (!startLocation.isNull() && !(queryFlags() & QueryMessage::NoSortReferencesByInput)) {
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
        if (count)
            return 0;
    }
    return 1;
}
