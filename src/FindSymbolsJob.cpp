#include "Database.h"
#include "FindSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::WriteUnfiltered|Job::QuoteOutput : Job::WriteUnfiltered;
}

FindSymbolsJob::FindSymbolsJob(const QueryMessage &query)
    : Job(query, ::jobFlags(query.flags())), string(query.query())
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


void FindSymbolsJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::SymbolName, Server::Read);
    // const bool hasFilter = !pathFilters().isEmpty();

    RTags::Ptr<Iterator> it(db->createIterator());

    if (string.isEmpty()) {
        it->seekToFirst();
    } else {
        it->seek(string.constData());
    }
    Set<Location> out;
    while (it->isValid() && !isAborted()) {
        const ByteArray entry = it->key().byteArray();
        const int cmp = strcmp(string.constData(), entry.constData());
        if (!cmp) {
            const Set<Location> locations = it->value<Set<Location> >();
            out += locations;
        } else if (cmp > 0) {
            break;
        }
        it->next();
    }
    List<LocationAndDefinitionNode> sorted;
    sorted.reserve(out.size());
    db = Server::instance()->db(Server::Symbol, Server::Read);
    for (Set<Location>::const_iterator it = out.begin(); it != out.end(); ++it) {
        sorted.push_back(LocationAndDefinitionNode(*it, RTags::findCursorInfo(db, *it).isDefinition));
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


