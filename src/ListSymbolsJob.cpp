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

#include "ListSymbolsJob.h"
#include "Server.h"
#include "Project.h"
#include <rct/Log.h>
#include "RTags.h"

enum {
    DefaultFlags = QueryJob::WriteUnfiltered|QueryJob::QuietJob,
    ElispFlags = DefaultFlags|QueryJob::QuoteOutput
};


ListSymbolsJob::ListSymbolsJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &proj)
    : QueryJob(query, query->flags() & QueryMessage::ElispList ? ElispFlags : DefaultFlags, proj),
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
        } else {
            out = listSymbols(proj);
        }
    }

    const bool elispList = queryFlags() & QueryMessage::ElispList;

    if (elispList) {
        write("(list", IgnoreMax|DontQuote);
        for (Set<String>::const_iterator it = out.begin(); it != out.end(); ++it) {
            write(*it);
        }
        write(")", IgnoreMax|DontQuote);
    } else {
        List<String> sorted = out.toList();
        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<String>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        const int count = sorted.size();
        for (int i=0; i<count; ++i) {
            write(sorted.at(i));
        }
    }
    return out.isEmpty() ? 1 : 0;
}

Set<String> ListSymbolsJob::imenu(const std::shared_ptr<Project> &project)
{
    Set<String> out;

    const SymbolMap &map = project->symbols();
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
        for (SymbolMap::const_iterator it = map.lower_bound(Location(fileId, 1, 0));
             it != map.end() && it->first.fileId() == fileId; ++it) {
            const std::shared_ptr<CursorInfo> &cursorInfo = it->second;
            if (RTags::isReference(cursorInfo->kind))
                continue;
            switch (cursorInfo->kind) {
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_InclusionDirective:
            case CXCursor_EnumConstantDecl:
                break;
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_ClassTemplate:
                if (!cursorInfo->isDefinition())
                    break;
                // fall through
            default: {
                const String &symbolName = it->second->symbolName;
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
    Set<String> out;
    const bool hasFilter = QueryJob::hasFilter();
    const bool stripParentheses = queryFlags() & QueryMessage::StripParentheses;
    const bool wildcard = queryFlags() & QueryMessage::WildcardSymbolNames && (string.contains('*') || string.contains('?'));
    const bool caseInsensitive = queryFlags() & QueryMessage::MatchCaseInsensitive;
    const String::CaseSensitivity cs = caseInsensitive ? String::CaseInsensitive : String::CaseSensitive;
    String lowerBound;
    // error() << "SHOBA" << wildcard << string;
    if (wildcard) {
        if (!string.endsWith('*'))
            string.append('*');
        if (!caseInsensitive) {
            for (int i=0; i<string.size(); ++i) {
                if (string.at(i) == '?' || string.at(i) == '*') {
                    lowerBound = string.left(i);
                    break;
                }
            }
        }
    } else if (!caseInsensitive) {
        lowerBound = string;
    }

    const SymbolNameMap &map = project->symbolNames();
    SymbolNameMap::const_iterator it = string.isEmpty() || caseInsensitive ? map.begin() : map.lower_bound(lowerBound);
    int count = 0;
    while (it != map.end()) {
        const String &entry = it->first;
        ++it;
        if (!string.isEmpty()) {
            if (wildcard) {
                if (!Rct::wildCmp(string.constData(), entry.constData(), cs)) {
                    continue;
                }
            } else if (!entry.startsWith(string, cs)) {
                if (!caseInsensitive) {
                    break;
                } else {
                    continue;
                }
            }
        }
        bool ok = true;
        if (hasFilter) {
            ok = false;
            const Set<Location> &locations = it->second;
            for (Set<Location>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                if (filter(i->path())) {
                    ok = true;
                    break;
                }
            }
        }
        if (ok) {
            const int paren = entry.indexOf('(');
            if (paren == -1) {
                out.insert(entry);
            } else {
                out.insert(entry.left(paren));
                if (!stripParentheses)
                    out.insert(entry);
            }
        }
        if (!(++count % 100) && isAborted())
            break;
    }
    return out;
}
