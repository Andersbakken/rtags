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

#include "ListSymbolsJob.h"
#include "Server.h"
#include "Project.h"
#include <rct/Log.h>
#include "RTags.h"

const Flags<QueryJob::JobFlag> defaultFlags = (QueryJob::WriteUnfiltered | QueryJob::QuietJob);
const Flags<QueryJob::JobFlag> elispFlags = (defaultFlags | QueryJob::QuoteOutput);

ListSymbolsJob::ListSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj, query->flags() & QueryMessage::ElispList ? elispFlags : defaultFlags),
      string(query->query())
{
}

int ListSymbolsJob::execute()
{
    Set<String> out;
    std::shared_ptr<Project> proj = project();
    if (proj) {
        if (queryFlags() & QueryMessage::IMenu) {
            out = imenu(proj);
        }
        else {
            out = listSymbols(proj);
        }
    }

    const bool elispList = queryFlags() & QueryMessage::ElispList;

    if (elispList) {
        write("(list", IgnoreMax | DontQuote);
        for (Set<String>::const_iterator it = out.begin(); it != out.end(); ++it) {
            write(*it);
        }
        write(")", IgnoreMax | DontQuote);
    }
    else {
        List<String> sorted = out.toList();
        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<String>());
        }
        else {
            std::sort(sorted.begin(), sorted.end());
        }
        const int count = sorted.size();
        for (int i = 0; i < count; ++i) {
            write(sorted.at(i));
        }
    }
    return out.isEmpty() ? 1 : 0;
}

Set<String> ListSymbolsJob::imenu(const std::shared_ptr<Project> &project)
{
    Set<String> out;
    const List<String> paths = pathFilters();
    if (paths.isEmpty()) {
        error() << "--imenu must take path filters";
        return out;
    }

    for (int i=0; i<paths.size(); ++i) {
        const Path file = paths.at(i);
        if (!file.isFile()) {
            error() << "Invalid path filter for --imenu" << file;
            continue;
        }
        const uint32_t fileId = Location::fileId(file);
        if (!fileId)
            continue;
        auto symbols = project->openSymbols(fileId);
        if (!symbols)
            continue;
        const int count = symbols->count();
        for (int j=0; j<count; ++j) {
            const Symbol &symbol = symbols->valueAt(j);
            if (RTags::isReference(symbol.kind))
                continue;
            switch (symbol.kind) {
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_InclusionDirective:
            case CXCursor_EnumConstantDecl:
                break;
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_ClassTemplate:
                if (!symbol.isDefinition())
                    break;
                // fall through
            default: {
                const String &symbolName = symbol.symbolName;
                if (!string.isEmpty() && !symbolName.contains(string))
                    continue;
                out.insert(symbolName);
                break; }
            }
        }
    }
    return out;
}

Set<String> ListSymbolsJob::listSymbols(const std::shared_ptr<Project> &project)
{
    const bool hasFilter = QueryJob::hasFilter();
    const bool stripParentheses = queryFlags() & QueryMessage::StripParentheses;
    Set<String> out;
    auto inserter = [this, hasFilter, stripParentheses, &out](Project::SymbolMatchType,
                                                              const String &string,
                                                              const Set<Location> &locations) {
        if (hasFilter) {
            bool ok = false;
            for (const auto &l : locations) {
                if (filter(l.path())) {
                    ok = true;
                    break;
                }
            }
            if (!ok)
                return;
        }
        const int paren = string.indexOf('(');
        if (paren == -1) {
            out.insert(string);
        } else {
            if (!RTags::isFunctionVariable(string))
                out.insert(string.left(paren));
            if (!stripParentheses)
                out.insert(string);
        }
    };
    if (queryFlags() & QueryMessage::WildcardSymbolNames
        && (string.contains('*') || string.contains('?'))
        && !string.endsWith('*'))
        string += '*';

    project->findSymbols(string, inserter, queryFlags());
    return out;
}
