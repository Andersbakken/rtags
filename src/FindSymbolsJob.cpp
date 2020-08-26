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

#include "FindSymbolsJob.h"

#include <stddef.h>

#include "Project.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "Symbol.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Set.h"

class Location;

static inline Flags<QueryJob::JobFlag> jobFlags(Flags<QueryMessage::Flag> queryFlags)
{
    return (queryFlags & QueryMessage::Elisp
            ? Flags<QueryJob::JobFlag>(QueryJob::QuoteOutput|QueryJob::QuietJob)
            : Flags<QueryJob::JobFlag>(QueryJob::QuietJob));
}

FindSymbolsJob::FindSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj, ::jobFlags(query->flags())), string(query->query())
{
}

int FindSymbolsJob::execute()
{
    const bool stripParentheses = queryFlags() & QueryMessage::StripParentheses;
    int ret = 2;
    if (std::shared_ptr<Project> proj = project()) {
        Set<Symbol> symbols;
        auto inserter = [proj, this, stripParentheses, &symbols](Project::SymbolMatchType type,
                                                                 const String &symbolName,
                                                                 const Set<Location> &locations) {
            if (type == Project::StartsWith) {
                const size_t paren = symbolName.indexOf('(');
                if (paren == String::npos || paren != string.size() || RTags::isFunctionVariable(symbolName))
                    return;
            }
            if (stripParentheses) {
                const size_t paren = symbolName.indexOf('(');
                if (paren != String::npos && RTags::isFunctionVariable(symbolName)) {
                    return;
                }
            }
            for (const auto &it : locations) {
                const Symbol sym = proj->findSymbol(it);
                if (!sym.isNull() || sym.flags & Symbol::FileSymbol)
                    symbols.insert(sym);
            }
        };
        proj->findSymbols(string, inserter, queryFlags(), fileFilter());
        if (!symbols.isEmpty()) {
            const List<RTags::SortedSymbol> sorted = proj->sort(symbols, queryFlags());
            const Flags<WriteFlag> writeFlags = fileFilter() ? Unfiltered : NoWriteFlags;
            const int count = sorted.size();
            ret = count ? 0 : 1;
            for (int i=0; i<count; ++i) {
                write(sorted.at(i).location, writeFlags);
            }
        }
    }
    return ret;
}
