#include "FindSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::QuoteOutput : Job::None;
}

FindSymbolsJob::FindSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, ::jobFlags(query.flags()), proj), string(query.query())
{
}

struct LocationAndDefinitionNode
{
    LocationAndDefinitionNode(const Location &loc, bool def)
        : location(loc), isDefinition(def)
    {}
    LocationAndDefinitionNode() {}
    Location location;
    bool isDefinition;

    bool operator<(const LocationAndDefinitionNode &other) const
    {
        if (isDefinition != other.isDefinition)
            return isDefinition;
        return location < other.location;
    }
    bool operator>(const LocationAndDefinitionNode &other) const
    {
        if (isDefinition != other.isDefinition)
            return !isDefinition;
        return location > other.location;
    }
};

static inline bool isDefinition(const SymbolMap &symbols, const Location &loc)
{
    const SymbolMap::const_iterator it = symbols.find(loc);
    return it != symbols.end() && it->second.isDefinition;
}

void FindSymbolsJob::execute()
{
    Map<Location, bool> out;
    // if (!(queryFlags() & QueryMessage::DisableGRTags) && (project()->grtags->flags() & GRTags::Parse)) {
    //     ScopedDB database = db(Project::GR, ReadWriteLock::Read);
    //     RTags::Ptr<Iterator> it(database->createIterator());

    //     if (string.isEmpty()) {
    //         it->seekToFirst();
    //     } else {
    //         it->seek(string.constData());
    //     }
    //     while (it->isValid() && !isAborted()) {
    //         const ByteArray entry = it->key().byteArray();
    //         const int cmp = strcmp(string.constData(), entry.constData());
    //         if (!cmp) {
    //             const Map<Location, bool> locations = it->value<Map<Location, bool> >();
    //             for (Map<Location, bool>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
    //                 if (!i->second) {
    //                     out[i->first] = false;
    //                 }
    //             }
    //         } else if (cmp > 0) {
    //             break;
    //         }
    //         it->next();
    //     }
    // }

    if (project()->indexer) {
        Scope<const SymbolNameMap &> scope = project()->lockSymbolNamesForRead();
        const SymbolNameMap &map = scope.data();
        const SymbolNameMap::const_iterator it = map.find(string);
        if (it != map.end()) {
            const Set<Location> &locations = it->second;
            for (Set<Location>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                out[*i] = true;
            }
        }
    }
    if (out.size()) {
        Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
        const SymbolMap &map = scope.data();
        List<LocationAndDefinitionNode> sorted;
        sorted.reserve(out.size());
        for (Map<Location, bool>::const_iterator it = out.begin(); it != out.end(); ++it) {
            sorted.push_back(LocationAndDefinitionNode(it->first,
                                                       it->second && isDefinition(map, it->first)));
        }

        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<LocationAndDefinitionNode>());
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


