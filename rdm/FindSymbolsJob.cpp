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
    : Job(i, QueryJobPriority), string(query.query().front()), queryFlags(query.flags())
{
    setPathFilters(query.pathFilters(), queryFlags & QueryMessage::FilterSystemIncludes);
}

void FindSymbolsJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Read);
    // const bool hasFilter = !pathFilters().isEmpty();

    RTags::Ptr<Iterator> it(db->createIterator());

    if (string.isEmpty()) {
        it->seekToFirst();
    } else {
        it->seek(string.constData());
    }
    QSet<Location> out;
    while (it->isValid() && !isAborted()) {
        const ByteArray entry = it->key().byteArray();
        const int cmp = strcmp(string.constData(), entry.constData());
        if (!cmp) {
            const QSet<Location> locations = it->value<QSet<Location> >();
            out += locations;
        } else if (cmp > 0) {
            break;
        }
        it->next();
    }
    QList<Location> sorted = out.toList();
    if (queryFlags & QueryMessage::ReverseSort) {
        qSort(sorted.begin(), sorted.end(), qGreater<Location>());
    } else {
        qSort(sorted);
    }
    const quint32 keyFlags = QueryMessage::keyFlags(queryFlags);
    foreach (const Location &loc, sorted) {
        write(loc.key(keyFlags));
    }
}


