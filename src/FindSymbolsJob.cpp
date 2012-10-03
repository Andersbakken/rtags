#include "FindSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTagsClang.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::QuoteOutput : Job::None;
}

FindSymbolsJob::FindSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, ::jobFlags(query.flags()), proj), string(query.query())
{
}

void FindSymbolsJob::execute()
{
    Map<Location, bool> out;
    if (project()->indexer) {
        Scope<const SymbolNameMap&> scope = project()->lockSymbolNamesForRead();
        if (scope.isNull())
            return;
        const SymbolNameMap &map = scope.data();
        const SymbolNameMap::const_iterator it = map.find(string);
        if (it != map.end()) {
            const Set<Location> &locations = it->second;
            for (Set<Location>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                out[*i] = true;
            }
        }
    }
    if (project()->grtags) {
        Scope<const GRMap &> scope = project()->lockGRForRead();
        const GRMap &map = scope.data();
        GRMap::const_iterator it = map.find(string);
        if (it != map.end()) {
            const Map<Location, bool> &locations = it->second;
            for (Map<Location, bool>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                if (!i->second)
                    out[i->first] = false;
            }
        }
    }

    if (out.size()) {
        Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
        const SymbolMap *map = &scope.data();
        List<RTags::SortedCursor> sorted;
        sorted.reserve(out.size());
        for (Map<Location, bool>::const_iterator it = out.begin(); it != out.end(); ++it) {
            RTags::SortedCursor node(it->first);
            if (it->second && map) {
                const SymbolMap::const_iterator found = map->find(it->first);
                if (found != map->end()) {
                    node.isDefinition = found->second.isDefinition;
                    node.kind = found->second.kind;
                }
            }
            sorted.push_back(node);
        }

        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedCursor>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        const uint32_t keyFlags = QueryMessage::keyFlags(queryFlags());
        const int count = sorted.size();
        for (int i=0; i<count; ++i) {
            write(sorted.at(i).location.key(keyFlags));
        }
    }
}
