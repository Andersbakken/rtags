/* This file is part of RTags (http://rtags.net).

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
    : QueryJob(query, proj)
{
    locations.insert(loc);
}

ReferencesJob::ReferencesJob(const String &sym, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj), symbolName(sym)
{
}

int ReferencesJob::execute()
{
    const bool rename = queryFlags() & QueryMessage::Rename;
    std::shared_ptr<Project> proj = project();
    if (!proj)
        return 1;
    Set<Symbol> refs;
    Map<Location, std::pair<bool, CXCursorKind> > references;
    if (!symbolName.isEmpty()) {
        const bool hasFilter = QueryJob::hasFilter();
        auto inserter = [this, hasFilter](Project::SymbolMatchType type, const String &string, const Set<Location> &locs) {
            if (type == Project::StartsWith) {
                const int paren = string.indexOf('(');
                if (paren == -1 || paren != symbolName.size() || RTags::isFunctionVariable(string))
                    return;
            }

            for (const auto &l : locs) {
                if (!hasFilter || filter(l.path())) {
                    locations.insert(l);
                }
            }
        };
        proj->findSymbols(symbolName, inserter, queryFlags());
    }
    const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
    Location startLocation;
    bool first = true;
    for (auto it = locations.begin(); it != locations.end(); ++it) {
        const Location pos = *it;
        Symbol sym = proj->findSymbol(pos);
        if (sym.isNull())
            continue;
        if (first && !(queryFlags() & QueryMessage::NoSortReferencesByInput)) {
            first = false;
            startLocation = sym.location;
        }

        if (sym.isReference())
            sym = proj->findTarget(sym);
        if (sym.isNull())
            continue;

        if (rename && sym.isConstructorOrDestructor()) {
            const Location loc = sym.location;
            sym.clear();
            const Set<String> usrs = proj->findTargetUsrs(loc);
            for (const String &usr : usrs) {
                for (const Symbol &s : proj->findByUsr(usr, loc.fileId(), Project::ArgDependsOn)) {
                    if (s.isClass()) {
                        sym = s;
                        if (s.isDefinition())
                            break;
                    }
                }
            }

            if (sym.isNull())
                continue;
        }
        if (queryFlags() & QueryMessage::AllReferences) {
            const Set<Symbol> all = proj->findAllReferences(sym);
            for (const auto &symbol : all) {
                if (rename) {
                    if (symbol.kind == CXCursor_MacroExpansion && sym.kind != CXCursor_MacroDefinition)
                        continue;
                    if (symbol.flags & Symbol::AutoRef)
                        continue;
                } else if (sym.isClass() && symbol.isConstructorOrDestructor()) {
                    continue;
                }
                const bool def = symbol.isDefinition();
                if (def && declarationOnly)
                    continue;
                references[symbol.location] = std::make_pair(def, symbol.kind);
            }
        } else if (queryFlags() & QueryMessage::FindVirtuals) {
            const Set<Symbol> virtuals = proj->findVirtuals(sym);
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
    if (rename) {
        if (!references.isEmpty()) {
            if (queryFlags() & QueryMessage::ReverseSort) {
                Map<Location, std::pair<bool, CXCursorKind> >::const_iterator it = references.end();
                do {
                    --it;
                    write(it->first);
                } while (it != references.begin());
            } else {
                for (const auto &it : references) {
                    write(it.first);
                }
            }
            return 0;
        }
    } else {
        List<RTags::SortedSymbol> sorted;
        sorted.reserve(references.size());
        for (Map<Location, std::pair<bool, CXCursorKind> >::const_iterator it = references.begin();
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
