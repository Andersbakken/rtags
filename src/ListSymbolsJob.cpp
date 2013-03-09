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

namespace foo
{
namespace bar
{
void foo();
struct F
{
    void f() {}
};

}

}

List<String> ListSymbolsJob::imenu(const shared_ptr<Project> &project)
{
    List<String> out;
    const bool elispList = queryFlags() & QueryMessage::ElispList;

    Scope<const SymbolMap&> symbols = project->lockSymbolsForRead();
    Scope<const SymbolNameMap&> symbolNames = project->lockSymbolNamesForRead();
    const SymbolNameMap &symbolNamesMap = symbolNames.data();
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
            case CXCursor_ClassDecl:
            case CXCursor_StructDecl:
            case CXCursor_ClassTemplate:
                if (cursorInfo.isDefinition())
                    break;
                // fall through
            case CXCursor_VarDecl:
            case CXCursor_ParmDecl:
            case CXCursor_InclusionDirective:
                continue;
            default:
                break;
            }
            String symbolName = it->second.symbolName;
            if (!string.isEmpty() && !symbolName.contains(string))
                continue;

            int last = symbolName.indexOf('(');
            while (true) {
                const int colons = symbolName.lastIndexOf("::", last);
                if (colons == -1)
                    break;

                const String candidate = symbolName.left(colons);

                error() << candidate << symbolName;
                const SymbolNameMap::const_iterator it = symbolNamesMap.find(candidate);
                if (it != symbolNamesMap.end() && !it->second.isEmpty()) {
                    const Set<Location> &locations = it->second;
                    error() << locations;
                    bool isNamespace = true;
                    for (Set<Location>::const_iterator loc = locations.begin(); loc != locations.end(); ++loc) {
                        const SymbolMap::const_iterator c = map.find(*loc);
                        if (c != map.end() && c->second.kind != CXCursor_Namespace) {
                            isNamespace = false;
                            break;
                        }
                    }
                    if (isNamespace) {
                        symbolName = symbolName.mid(colons + 2);
                        break;
                    }
                }
                last = colons - 2;
            }

            if (elispList) {
                write(symbolName);
            } else {
                out.append(symbolName);
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
    const bool skipParentheses = queryFlags() & QueryMessage::SkipParentheses;

    Scope<const SymbolNameMap&> symbolNames = project->lockSymbolNamesForRead();
    const SymbolNameMap &map = symbolNames.data();
    SymbolNameMap::const_iterator it = string.isEmpty() ? map.begin() : map.lower_bound(string);
    int count = 0;
    while (it != map.end()) {
        const String &entry = it->first;
        ++it;
        if (!string.isEmpty() && !entry.startsWith(string))
            break;
        if (skipParentheses && !entry.contains('('))
            continue;
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
            if (elispList) {
                write(entry);
            } else {
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

