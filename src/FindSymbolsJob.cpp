#include "Database.h"
#include "FindSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::WriteUnfiltered|Job::QuoteOutput : Job::WriteUnfiltered;
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


void FindSymbolsJob::execute()
{
    ScopedDB symbolDB;
    Map<Location, bool> out;
    if ((queryFlags() & QueryMessage::EnableGRTags) && (project()->grtags->flags() & GRTags::Parse)) {
        ScopedDB database = db(Project::GR, ReadWriteLock::Read);
        RTags::Ptr<Iterator> it(database->createIterator());

        if (string.isEmpty()) {
            it->seekToFirst();
        } else {
            it->seek(string.constData());
        }
        while (it->isValid() && !isAborted()) {
            const ByteArray entry = it->key().byteArray();
            const int cmp = strcmp(string.constData(), entry.constData());
            if (!cmp) {
                const Map<Location, bool> locations = it->value<Map<Location, bool> >();
                for (Map<Location, bool>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                    if (!i->second) {
                        out[i->first] = false;
                    }
                }
            } else if (cmp > 0) {
                break;
            }
            it->next();
        }
    }

    if (project()->indexer) {
        ScopedDB database = db(Project::SymbolName, ReadWriteLock::Read);
        // const bool hasFilter = !pathFilters().isEmpty();

        RTags::Ptr<Iterator> it(database->createIterator());

        if (string.isEmpty()) {
            it->seekToFirst();
        } else {
            it->seek(string.constData());
        }
        while (it->isValid() && !isAborted()) {
            const ByteArray entry = it->key().byteArray();
            const int cmp = strcmp(string.constData(), entry.constData());
            if (!cmp) {
                const Set<Location> locations = it->value<Set<Location> >();
                for (Set<Location>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                    out[*i] = true;
                }
            } else if (cmp > 0) {
                break;
            }
            it->next();
        }
        symbolDB = db(Project::Symbol, ReadWriteLock::Read);
    }

    if (out.size()) {
        List<LocationAndDefinitionNode> sorted;
        sorted.reserve(out.size());
        for (Map<Location, bool>::const_iterator it = out.begin(); it != out.end(); ++it) {
            sorted.push_back(LocationAndDefinitionNode(it->first, it->second ? RTags::findCursorInfo(symbolDB, it->first).isDefinition : false));
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


