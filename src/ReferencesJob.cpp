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
    if (!proj)
        return 1;
    Set<Symbol> refs;
    Map<Location, std::pair<bool, uint16_t> > references;
    if (!symbolName.isEmpty())
        locations = proj->locations(symbolName);
    const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
    Location startLocation;
    for (auto it = locations.begin(); it != locations.end(); ++it) {
        const Location pos = *it;
        if (it == locations.begin() && !(queryFlags() & QueryMessage::NoSortReferencesByInput))
            startLocation = pos;
        Symbol cursor = proj->findSymbol(pos);
        if (cursor.isNull())
            continue;
        if (cursor.isReference())
            cursor = proj->findTarget(cursor);
        if (cursor.isNull())
            continue;
        if (queryFlags() & QueryMessage::AllReferences) {
            const Set<Symbol> all = proj->findAllReferences(cursor);
            for (const auto &symbol : all) {
                const bool def = symbol.isDefinition();
                if (!declarationOnly || !def) {
                    references[symbol.location] = std::make_pair(def, symbol.kind);
                }
            }
#if 0
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
#endif
        } else if (queryFlags() & QueryMessage::FindVirtuals) {
            const Set<Symbol> virtuals = proj->findVirtuals(cursor);
            const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
            for (const auto &symbol : virtuals) {
                const bool def = symbol.isDefinition();
                if (!declarationOnly || !def)
                    references[symbol.location] = std::make_pair(def, symbol.kind);
            }
            startLocation.clear();
            // since one normally calls this on a declaration it kinda
            // doesn't work that well to do the clever offset thing
            // underneath
        } else {
            const Set<Symbol> symbols = proj->findCallers(pos);
            const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
            for (const auto &symbol : symbols) {
                const bool def = symbol.isDefinition();
                if (!declarationOnly || !def)
                    references[symbol.location] = std::make_pair(false, CXCursor_FirstInvalid);
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
        List<RTags::SortedSymbol> sorted;
        sorted.reserve(references.size());
        for (Map<Location, std::pair<bool, uint16_t> >::const_iterator it = references.begin();
             it != references.end(); ++it) {
            sorted.append(RTags::SortedSymbol(it->first, it->second.first, it->second.second));
        }
        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedSymbol>());
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
        if (count)
            return 0;
    }
    return 1;
}
