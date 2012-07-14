#include "Database.h"
#include "ListSymbolsJob.h"
#include "Server.h"
#include "ScopedDB.h"
#include "Log.h"
#include "RTags.h"

ListSymbolsJob::ListSymbolsJob(int i, const QueryMessage &query)
    : Job(query, query.flags() & QueryMessage::ElispList ? Job::QuoteOutput : Job::None), string(query.query())
{
    setId(i);
}

void ListSymbolsJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::SymbolName, ReadWriteLock::Read);
    const bool hasFilter = !pathFilters().isEmpty();
    const unsigned queryFlags = Job::queryFlags();
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
                for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
                    if (filter(it->path())) {
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
        std::sort(out.begin(), out.end(), std::greater<ByteArray>());
    } else {
        std::sort(out.begin(), out.end());
    }
    const int count = out.size();
    for (int i=0; i<count; ++i) {
        write(out.at(i));
    }
}


