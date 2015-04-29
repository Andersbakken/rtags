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

#include "FindSymbolsJob.h"
#include "Server.h"
#include <rct/Log.h>
#include "RTagsClang.h"
#include "Project.h"

static inline Flags<QueryJob::JobFlag> jobFlags(Flags<QueryMessage::Flag> queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? QueryJob::QuoteOutput|QueryJob::QuietJob : QueryJob::QuietJob;
}

FindSymbolsJob::FindSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj, ::jobFlags(query->flags())), string(query->query())
{
}

int FindSymbolsJob::execute()
{
    int ret = 2;
    if (std::shared_ptr<Project> proj = project()) {
        Set<Symbol> symbols;
        const uint32_t filter = fileFilter();
        auto inserter = [proj, this, &symbols](Project::SymbolMatchType type, const String &symbolName, const Set<Location> &locations) {
            if (type == Project::StartsWith) {
                const int paren = symbolName.indexOf('(');
                if (paren == -1 || paren != string.size() || RTags::isFunctionVariable(symbolName))
                    return;
            }
            for (const auto &it : locations) {
                const Symbol sym = proj->findSymbol(it);
                if (!sym.isNull())
                    symbols.insert(sym);
            }
        };
        proj->findSymbols(string, inserter, queryFlags());
        if (!symbols.isEmpty()) {
            Flags<Project::SortFlag> sortFlags = Project::Sort_None;
            if (queryFlags() & QueryMessage::DeclarationOnly)
                sortFlags |= Project::Sort_DeclarationOnly;
            if (queryFlags() & QueryMessage::ReverseSort)
                sortFlags |= Project::Sort_Reverse;

            const List<RTags::SortedSymbol> sorted = proj->sort(symbols, sortFlags);
            const Flags<WriteFlag> writeFlags = filter ? NoWriteFlags : Unfiltered;
            const int count = sorted.size();
            ret = count ? 0 : 1;
            for (int i=0; i<count; ++i) {
                write(sorted.at(i).location, writeFlags);
            }
        }
    }
    return ret;
}
