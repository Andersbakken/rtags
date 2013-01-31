#include "ListSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

enum {
    DefaultFlags = Job::WriteUnfiltered|Job::WriteBuffered,
    ElispFlags = DefaultFlags|Job::QuoteOutput
};


ListSymbolsJob::ListSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, query.flags() & QueryMessage::ElispList ? ElispFlags : DefaultFlags, proj),
      string(query.query())
{
}

void ListSymbolsJob::execute()
{
    List<String> out;
    const bool hasFilter = Job::hasFilter();
    const unsigned queryFlags = Job::queryFlags();
    const bool skipParentheses = queryFlags & QueryMessage::SkipParentheses;
    const bool elispList = queryFlags & QueryMessage::ElispList;

    if (elispList)
        write("(list", IgnoreMax|DontQuote);
    shared_ptr<Project> proj = project();
    if (proj) {
        Scope<const SymbolNameMap&> scope = proj->lockSymbolNamesForRead();
        if (scope.isNull())
            return;
        const SymbolNameMap &map = scope.data();
        SymbolNameMap::const_iterator it = string.isEmpty() ? map.begin() : map.lower_bound(string);
        int count = 0;
        while (it != map.end()) {
            const String &entry = it->first;
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
            if (!(++count % 10) && isAborted())
                return;
        }
    }

    if (elispList) {
        write(")", IgnoreMax|DontQuote);
    } else {
        if (queryFlags & QueryMessage::ReverseSort) {
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
