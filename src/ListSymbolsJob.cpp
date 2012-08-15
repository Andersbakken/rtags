#include "Database.h"
#include "ListSymbolsJob.h"
#include "Server.h"
#include "ScopedDB.h"
#include "Log.h"
#include "RTags.h"

ListSymbolsJob::ListSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, query.flags() & QueryMessage::ElispList ? Job::QuoteOutput : Job::None, proj), string(query.query())
{
}

void ListSymbolsJob::execute()
{
    List<ByteArray> out;
    const bool hasFilter = !pathFilters().isEmpty();
    const unsigned queryFlags = Job::queryFlags();
    const bool skipParentheses = queryFlags & QueryMessage::SkipParentheses;
    const bool elispList = queryFlags & QueryMessage::ElispList;

    if (project()->indexer) {
        ScopedDB database = db(Project::SymbolName, ReadWriteLock::Read);
        RTags::Ptr<Iterator> it = database->createIterator();
        if (string.isEmpty()) {
            it->seekToFirst();
        } else {
            it->seek(string.constData());
        }
        if (elispList)
            writeRaw("(list");
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
    }
    if (queryFlags & QueryMessage::EnableGRTags && project()->grtags) { // grtags can become non-null at any given time
        ScopedDB database = db(Project::GR, ReadWriteLock::Read);
        RTags::Ptr<Iterator> it = database->createIterator();
        it.reset(database->createIterator());
        if (string.isEmpty()) {
            it->seekToFirst();
        } else {
            it->seek(string.constData());
        }
        while (it->isValid() && !isAborted()) {
            const ByteArray entry = it->key().byteArray();
            if (!string.isEmpty() && !entry.startsWith(string))
                break;
            if (!skipParentheses || !entry.contains('(')) {
                const Map<Location, bool> locations = it->value<Map<Location, bool> >();
                error() << entry << locations.size();
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
            }
            it->next();
        }
    }

    if (elispList) {
        writeRaw(")");
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


