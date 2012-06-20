#include "Database.h"
#include "ListSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"
#include "Rdm.h"

ListSymbolsJob::ListSymbolsJob(int i, const QueryMessage &query)
    : Job(i, QueryJobPriority, query.flags() & QueryMessage::ElispList ? Job::QuoteOutput : Job::None),
      string(query.query().front()), queryFlags(query.flags())
{
    setPathFilters(query.pathFilters(), queryFlags & QueryMessage::FilterSystemIncludes);
}

void ListSymbolsJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Read);
    const bool hasFilter = !pathFilters().isEmpty();
    const bool skipParentheses = queryFlags & QueryMessage::SkipParentheses;
    const bool elispList = queryFlags & QueryMessage::ElispList;

    RTags::Ptr<Iterator> it(db->createIterator());
    if (string.isEmpty()) {
        it->seekToFirst();
    } else {
        it->seek(string.constData());
    }
    if (elispList)
        writeRaw("(list");
    List<ByteArray> out;
    while (it->isValid() && !isAborted()) {
        const ByteArray entry = it->key().byteArray();
        if (!string.isEmpty() && !entry.startsWith(string))
            break;
        if (!skipParentheses || !entry.contains('(')) {
            bool ok = true;
            if (hasFilter) {
                ok = false;
                const Set<Location> locations = it->value<Set<Location> >();
                foreach(const Location &loc, locations) {
                    if (filter(loc.path())) {
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
        it->next();
    }
    if (elispList) {
        writeRaw(")");
        return;
    }
    if (queryFlags & QueryMessage::ReverseSort) {
        qSort(out.begin(), out.end(), qGreater<ByteArray>());
    } else {
        qSort(out);
    }
    foreach (const ByteArray &o, out) {
        write(o);
    }
}


