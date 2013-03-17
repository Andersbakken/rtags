#include "ListSymbolsJob.h"
#include "Server.h"
#include "Project.h"
#include <rct/Log.h>
#include "RTags.h"

enum {
    DefaultFlags = Job::WriteUnfiltered|Job::WriteBuffered|Job::QuietJob,
    ElispFlags = DefaultFlags|Job::QuoteOutput|Job::QuietJob
};


ListSymbolsJob::ListSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, query.flags() & QueryMessage::ElispList ? ElispFlags : DefaultFlags, proj),
      string(query.query())
{
}

void ListSymbolsJob::execute()
{
    List<String> out;
    const bool elispList = queryFlags() & QueryMessage::ElispList;

    if (elispList)
        write("(list", IgnoreMax|DontQuote);
    shared_ptr<Project> proj = project();
    if (proj) {
        if (queryFlags() & QueryMessage::IMenu) {
            out = imenu(proj);
        } else {
            out = listSymbols(proj);
        }
    }

    if (elispList) {
        write(")", IgnoreMax|DontQuote);
    } else {
        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(out.begin(), out.end(), std::greater<String>());
        } else {
            std::sort(out.begin(), out.end());
        }
        const int count = out.size();
        for (int i=0; i<count; ++i) {
            write(out.at(i));
        }
    }
}

List<String> ListSymbolsJob::imenu(const shared_ptr<Project> &project)
{
    List<String> out;
    const bool elispList = queryFlags() & QueryMessage::ElispList;

    Scope<const SymbolMap&> symbols = project->lockSymbolsForRead();
    const SymbolMap &map = symbols.data();
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
        for (SymbolMap::const_iterator it = map.lower_bound(Location(fileId, 0));
             it != map.end() && it->first.fileId() == fileId; ++it) {
            const CursorInfo &cursorInfo = it->second;
            if (RTags::isReference(cursorInfo.kind))
                continue;
            switch (cursorInfo.kind) {
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_InclusionDirective:
            case CXCursor_EnumConstantDecl:
                break;
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_ClassTemplate:
                if (!cursorInfo.isDefinition())
                    break;
                // fall through
            default: {
                const String &symbolName = it->second.symbolName;
                if (!string.isEmpty() && !symbolName.contains(string))
                    continue;
                if (elispList) {
                    write(symbolName);
                } else {
                    out.append(symbolName);
                }
                break; }
            }
        }
    }
    return out;
}

List<String> ListSymbolsJob::listSymbols(const shared_ptr<Project> &project)
{
    List<String> out;
    const bool hasFilter = Job::hasFilter();
    const bool elispList = queryFlags() & QueryMessage::ElispList;
    const bool skipParentheses = queryFlags() & QueryMessage::StripParentheses;

    Set<String> have;

    Scope<const SymbolNameMap&> symbolNames = project->lockSymbolNamesForRead();
    const SymbolNameMap &map = symbolNames.data();
    SymbolNameMap::const_iterator it = string.isEmpty() ? map.begin() : map.lower_bound(string);
    int count = 0;
    while (it != map.end()) {
        const String &entry = it->first;
        ++it;
        if (!string.isEmpty() && !entry.startsWith(string))
            break;
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
            if (skipParentheses) {
                const int paren = entry.indexOf('(');
                if (paren == -1) {
                    if (have.contains(entry))
                        continue;
                    have.insert(entry);
                    if (elispList)
                        write(entry);
                    else
                        out.append(entry);
                } else {
                    const String noparen = entry.left(paren);
                    if (have.contains(noparen))
                        continue;
                    have.insert(noparen);
                    if (elispList)
                        write(noparen);
                    else
                        out.append(noparen);
                }
            } else {
                if (elispList)
                    write(entry);
                else
                    out.append(entry);
            }
        }
        if (!(++count % 10) && isAborted())
            break;
    }
    return out;
}

void ListSymbolsJob::foobar()
{}

