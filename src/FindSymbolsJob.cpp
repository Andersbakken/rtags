#include "Database.h"
#include "FindSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"
#include "Rdm.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::WriteUnfiltered|Job::QuoteOutput : Job::WriteUnfiltered;
}

FindSymbolsJob::FindSymbolsJob(int i, const QueryMessage &query)
    : Job(i, QueryJobPriority), string(query.query()), queryFlags(query.flags())
{
    setPathFilters(query.pathFilters(), queryFlags & QueryMessage::FilterSystemIncludes);
}

void FindSymbolsJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::SymbolName, ReadWriteLock::Read);
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
    List<Location> sorted = out.toList();
    if (queryFlags & QueryMessage::ReverseSort && false) {
        std::sort(sorted.begin(), sorted.end(), std::greater<Location>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    const uint32_t keyFlags = QueryMessage::keyFlags(queryFlags);
    const int count = sorted.size();
    for (int i=0; i<count; ++i) {
        const Location &loc = sorted.at(i);
        write(loc.key(keyFlags));
    }
}


