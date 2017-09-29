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

#include "Project.h"
#include "QueryMessage.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "RTags.h"
#include "Server.h"

const Flags<QueryJob::JobFlag> defaultFlags = (QueryJob::WriteUnfiltered | QueryJob::QuietJob);
const Flags<QueryJob::JobFlag> elispFlags = (defaultFlags | QueryJob::QuoteOutput);

ListSymbolsJob::ListSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, proj, query->flags() & QueryMessage::Elisp ? elispFlags : defaultFlags),
      string(query->query())
{
}

int ListSymbolsJob::execute()
{
    Set<String> out;
    std::shared_ptr<Project> proj = project();
    if (proj) {
        if (queryFlags() & QueryMessage::WildcardSymbolNames
            && (string.contains('*') || string.contains('?')) && !string.endsWith('*')) {
            string += '*';
        }
        List<QueryMessage::PathFilter> filters = pathFilters();
        List<Path> paths;
        for (const auto &filter : filters) {
            if (filter.mode == QueryMessage::PathFilter::Self) {
                paths.append(filter.pattern);
                if (!paths.last().isFile()) {
                    paths.clear();
                    break;
                }
            } else {
                paths.clear();
                break;
            }
        }
        if (!paths.isEmpty()) {
            out = listSymbolsWithPathFilter(proj, paths);
        } else {
            out = listSymbols(proj);
        }
    }

    if (queryFlags() & QueryMessage::Elisp) {
        write("(list", IgnoreMax | DontQuote);
        for (Set<String>::const_iterator it = out.begin(); it != out.end(); ++it) {
            write(*it);
        }
        write(")", IgnoreMax | DontQuote);
    } else {
        List<String> sorted = out.toList();
        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<String>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        const int count = sorted.size();
        for (int i = 0; i < count; ++i) {
            write(sorted.at(i));
        }
    }
    return out.isEmpty() ? 1 : 0;
}

Set<String> ListSymbolsJob::listSymbolsWithPathFilter(const std::shared_ptr<Project> &project, const List<Path> &paths) const
{
    Set<String> out;
    const bool wildcard = queryFlags() & QueryMessage::WildcardSymbolNames && (string.contains('*') || string.contains('?'));
    const bool stripParentheses = queryFlags() & QueryMessage::StripParentheses;
    const bool caseInsensitive = queryFlags() & QueryMessage::MatchCaseInsensitive;
    const String::CaseSensitivity cs = caseInsensitive ? String::CaseInsensitive : String::CaseSensitive;
    for (size_t i=0; i<paths.size(); ++i) {
        const Path file = paths.at(i);
        const uint32_t fileId = Location::fileId(file);
        if (!fileId)
            continue;
        auto symbols = project->openSymbols(fileId);
        if (!symbols)
            continue;
        const int count = symbols->count();
        for (int j=0; j<count; ++j) {
            const Symbol &symbol = symbols->valueAt(j);
            if (!filterKind(symbol)) {
                continue;
            }
            const String &symbolName = symbol.symbolName;
            if (symbolName.isEmpty())
                continue;
            if (!string.isEmpty()) {
                if (wildcard) {
                    if (!Project::matchSymbolName(string, symbolName, cs)) {
                        continue;
                    }
                } else if (!symbolName.contains(string, cs)) {
                    continue;
                }
            }

            if (stripParentheses) {
                const int paren = symbolName.indexOf('(');
                if (paren == -1) {
                    out.insert(symbolName);
                } else {
                    if (!RTags::isFunctionVariable(symbolName))
                        out.insert(symbolName.left(paren));
                }
            } else {
                out.insert(symbolName);
            }
        }
    }
    return out;
}

namespace {

struct SymbolHash
{
    std::size_t operator()(const Symbol &x) const
    {
        return std::hash<uint64_t>{}(x.location.value);
    }
};

struct SymbolEqual
{
    bool operator()(const Symbol &lhs, const Symbol &rhs) const
    {
        return lhs.location == rhs.location;
    }
};

} // namespace

Set<String> ListSymbolsJob::listSymbols(const std::shared_ptr<Project> &project) const
{
    const bool hasFilter = QueryJob::hasFilter();
    const bool hasKindFilter = QueryJob::hasKindFilter();
    const bool stripParentheses = queryFlags() & QueryMessage::StripParentheses;

    std::unordered_map<Symbol, String, SymbolHash, SymbolEqual> tmp;
    const auto insertOrUpdate = [&tmp](const Symbol &sym, const String &name) {
      auto it = tmp.insert({sym, name});
      if (it.second) { return; }
      if (name.size() < it.first->second.size()) { return; }
      it.first->second = name;
    };

    auto inserter = [
        this,
        &project,
        hasFilter,
        hasKindFilter,
        stripParentheses,
        &insertOrUpdate](Project::SymbolMatchType,
                         const String &str,
                         const Set<Location> &locations)
    {
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
        const Symbol sym = project->findSymbol(*locations.begin());
        if (hasKindFilter) {
            if (!filterKind(sym))
                return;
        }
        const auto paren = str.indexOf('(');
        if (paren == String::npos) {
            insertOrUpdate(sym, str);
        } else {
            if (!RTags::isFunctionVariable(str))
                insertOrUpdate(sym, str.left(paren));
            if (!stripParentheses)
                insertOrUpdate(sym, str);
        }
    };

    project->findSymbols(string, inserter, queryFlags());

    Set<String> out;
    for (const auto &x: tmp) {
        out.insert(x.second);
    }

    return out;
}
