#include "ListSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

enum {
    DefaultFlags = Job::WriteUnfiltered|Job::WriteBuffered,
    ElispFlags = DefaultFlags|Job::QuoteOutput
};


ListSymbolsJob::ListSymbolsJob(const QueryMessage &query, const std::shared_ptr<Project> &proj)
    : Job(query, query.flags() & QueryMessage::ElispList ? ElispFlags : DefaultFlags, proj),
      string(query.query())
{
}

void ListSymbolsJob::execute()
{
    List<ByteArray> out;
    const bool hasFilter = Job::hasFilter();
    const unsigned queryFlags = Job::queryFlags();
    const bool skipParentheses = queryFlags & QueryMessage::SkipParentheses;
    const bool elispList = queryFlags & QueryMessage::ElispList;

    if (elispList)
        write("(list", IgnoreMax|DontQuote);
    if (project()->indexer) {
        Scope<const SymbolNameMap&> scope = project()->lockSymbolNamesForRead();
        if (scope.isNull())
            return;
        const SymbolNameMap &map = scope.data();
        SymbolNameMap::const_iterator it = string.isEmpty() ? map.begin() : map.lower_bound(string);
        while (it != map.end()) {
            const ByteArray &entry = it->first;
            if (!string.isEmpty() && !entry.startsWith(string))
                break;
            if (!skipParentheses || !entry.contains('(')) {
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
            }
            ++it;
        }
    }

    if (project()->grtags) {
        Scope<const GRMap &> scope = project()->lockGRForRead();
        const GRMap &map = scope.data();
        GRMap::const_iterator it = string.isEmpty() ? map.begin() : map.lower_bound(string);
        while (it != map.end()) {
            const ByteArray &entry = it->first;
            if (!string.isEmpty() && !entry.startsWith(string))
                break;

            const Map<Location, bool> &locations = it->second;
            for (Map<Location, bool>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                if (!i->second && (!hasFilter || filter(i->first.path()))) {
                    if (elispList) {
                        write(entry);
                    } else {
                        out.append(entry);
                    }
                    break;
                }
            }
            ++it;
        }
    }

    if (elispList) {
        write(")", IgnoreMax|DontQuote);
    } else {
        if (queryFlags & QueryMessage::ReverseSort) {
            std::sort(out.begin(), out.end(), std::greater<ByteArray>());
        } else {
            std::sort(out.begin(), out.end());
        }
        const int count = out.size();
        for (int i=0; i<count; ++i) {
            write(out.at(i));
        }
    }
}
