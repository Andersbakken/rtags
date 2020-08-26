/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "ReferencesJob.h"

#include <stddef.h>
#include <algorithm>
#include <functional>
#include <map>
#include <utility>
#include <vector>

#include "Project.h"
#include "RTags.h"
#include "QueryMessage.h"
#include "Symbol.h"
#include "clang-c/Index.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Map.h"
#include "rct/Path.h"
#include "rct/Value.h"

static inline Flags<QueryJob::JobFlag> jobFlags(Flags<QueryMessage::Flag> queryFlags)
{
    return (queryFlags & QueryMessage::Elisp ? Flags<QueryJob::JobFlag>(QueryJob::QuoteOutput) : Flags<QueryJob::JobFlag>());
}

ReferencesJob::ReferencesJob(Location loc, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj, ::jobFlags(query->flags()))
{
    mLocations.insert(loc);
}

ReferencesJob::ReferencesJob(const String &sym, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj, ::jobFlags(query->flags())), mSymbolName(sym)
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
    if (!mSymbolName.isEmpty()) {
        const bool hasFilter = QueryJob::hasFilter();
        auto inserter = [this, hasFilter](Project::SymbolMatchType type, const String &string, const Set<Location> &locs) {
            if (type == Project::StartsWith) {
                const size_t paren = string.indexOf('(');
                if (paren == String::npos || paren != mSymbolName.size() || RTags::isFunctionVariable(string))
                    return;
            }

            for (const auto &l : locs) {
                if (!hasFilter || filter(l.path())) {
                    mLocations.insert(l);
                }
            }
        };
        proj->findSymbols(mSymbolName, inserter, queryFlags());
    }
    const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
    const bool definitionOnly = queryFlags() & QueryMessage::DefinitionOnly;
    Location startLocation;
    bool first = true;
    for (auto it = mLocations.begin(); it != mLocations.end(); ++it) {
        const Location pos = *it;
        Symbol sym = proj->findSymbol(pos);
        if (sym.isNull())
            continue;
        if (first && !(queryFlags() & QueryMessage::NoSortReferencesByInput)) {
            first = false;
            startLocation = sym.location;
        }

        if (sym.isReference()) {
            const Symbol target = proj->findTarget(sym);
            if (!target.isNull() && target.kind != CXCursor_MacroExpansion)
                sym = target;
        }
        if (sym.isNull())
            continue;

        if (rename && sym.isConstructorOrDestructor()) {
            const Location loc = sym.location;
            sym.clear();
            const Set<String> usrs = proj->findTargetUsrs(loc);
            for (const String &usr : usrs) {
                for (const Symbol &s : proj->findByUsr(usr, loc.fileId(), Project::All)) {
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
                    if (symbol.kind == CXCursor_MacroExpansion && sym.kind != CXCursor_MacroDefinition) {
                        continue;
                    }
                } else if (sym.isClass() && symbol.isConstructorOrDestructor()) {
                    continue;
                }
                const bool def = symbol.isDefinition();
                if (def) {
                    if (declarationOnly)
                        continue;
                } else if (definitionOnly) {
                    continue;
                }
                references[symbol.location] = std::make_pair(def, symbol.kind);
            }
        } else if (queryFlags() & QueryMessage::FindVirtuals) {
            const Set<Symbol> virtuals = proj->findVirtuals(sym);
            for (const auto &symbol : virtuals) {
                const bool def = symbol.isDefinition();
                if (def) {
                    if (declarationOnly)
                        continue;
                } else if (definitionOnly) {
                    continue;
                }
                references[symbol.location] = std::make_pair(def, symbol.kind);
            }
        } else {
            const Set<Symbol> symbols = proj->findCallers(sym);
            for (const auto &symbol : symbols) {
                const bool def = symbol.isDefinition();
                if (def) {
                    if (declarationOnly)
                        continue;
                } else if (definitionOnly) {
                    continue;
                }
                references[symbol.location] = std::make_pair(false, CXCursor_FirstInvalid);
            }
        }
    }
    Flags<QueryJob::WriteFlag> writeFlags;
    if (queryFlags() & QueryMessage::Elisp) {
        write("(list ", DontQuote);
        writeFlags |= QueryJob::NoContext;
    } else if (queryFlags() & QueryMessage::NoContext) {
        writeFlags |= QueryJob::NoContext;
    }

    auto writeCons = [this](const String &car, const String &cdr) {
        write("(cons ", DontQuote);
        write(car, DontQuote);
        write(cdr);
        write(")", DontQuote);
    };

    Value json;
    auto writeLoc = [this, writeCons, writeFlags, &json](Location loc) {
        if (queryFlags() & QueryMessage::Elisp) {
            if (!filterLocation(loc))
                return;
            write("(list ", DontQuote);
            locationToString(loc, [writeCons, this](LocationPiece piece, const String &string) {
                    switch (piece) {
                    case Piece_ContainingFunctionLocation:
                        if (queryFlags() & QueryMessage::ContainingFunctionLocation)
                            writeCons("'cfl", string);
                        break;
                    case Piece_ContainingFunctionName:
                        if (queryFlags() & QueryMessage::ContainingFunction)
                            writeCons("'cf", string);
                        break;
                    case Piece_Location:
                        writeCons("'loc", string);
                        break;
                    case Piece_Context:
                        if (!(queryFlags() & QueryMessage::NoContext))
                            writeCons("'ctx", string);
                        break;
                    case Piece_SymbolName:
                    case Piece_Kind:
                        break;
                    }
                });
            write(")", DontQuote);
        } else if (queryFlags() & QueryMessage::JSON) {
            if (!filterLocation(loc))
                return;
            Value value;
            locationToString(loc, [&value, this](LocationPiece piece, const String &string) {
                    switch (piece) {
                    case Piece_ContainingFunctionLocation:
                        if (queryFlags() & QueryMessage::ContainingFunctionLocation)
                            value["cfl"] = string;
                        break;
                    case Piece_ContainingFunctionName:
                        if (queryFlags() & QueryMessage::ContainingFunction)
                            value["cf"] = string;
                        break;
                    case Piece_Location:
                        value["loc"] = string;
                        break;
                    case Piece_Context:
                        if (!(queryFlags() & QueryMessage::NoContext))
                            value["ctx"] = string;
                        break;
                    case Piece_SymbolName:
                    case Piece_Kind:
                        break;
                    }
                });
            json.push_back(value);
        } else {
            write(loc, writeFlags);
        }
    };

    if (rename) {
        if (!references.isEmpty()) {
            if (queryFlags() & QueryMessage::ReverseSort) {
                Map<Location, std::pair<bool, CXCursorKind> >::const_iterator it = references.end();
                do {
                    --it;
                    writeLoc(it->first);
                } while (it != references.begin());
            } else {
                for (const auto &it : references) {
                    writeLoc(it.first);
                }
            }
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
            Location loc = sorted.at((startIndex + i) % count).location;
            writeLoc(loc);
        }
    }
    if (queryFlags() & QueryMessage::Elisp) {
        write(")", DontQuote);
    } else if (queryFlags() & QueryMessage::JSON) {
        write(json.toJSON(), DontQuote|Unfiltered);
    }

    return references.isEmpty() ? 1 : 0;
}
