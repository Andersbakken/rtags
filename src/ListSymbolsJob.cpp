#include "ListSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

ListSymbolsJob::ListSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, query.flags() & QueryMessage::ElispList ? Job::QuoteOutput|Job::WriteUnfiltered : Job::WriteUnfiltered, proj),
      string(query.query())
{
}

void ListSymbolsJob::execute()
{
    List<ByteArray> out;
    const bool hasFilter = Job::hasFilter();
    const unsigned queryFlags = Job::queryFlags();
    const bool skipParentheses = queryFlags & QueryMessage::SkipParentheses;
    const bool elispList = queryFlags & QueryMessage::ElispList;

    if (elispList)
        writeRaw("(list");
    if (project()->indexer) {
        Scope<const SymbolNameMap &> scope = project()->lockSymbolNamesForRead();
        const SymbolNameMap &map = scope.data();
        SymbolNameMap::const_iterator it = string.isEmpty() ? map.begin() : map.lower_bound(string);
        while (it != map.end()) {
            const ByteArray &entry = it->first;
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
        }
    }
    // if (!(queryFlags & QueryMessage::DisableGRTags) && (project()->grtags->flags() & GRTags::Parse)) {
    //     RTags::Ptr<Iterator> it = database->createIterator();
    //     it.reset(database->createIterator());
    //     if (string.isEmpty()) {
    //         it->seekToFirst();
    //     } else {
    //         it->seek(string.constData());
    //     }
    //     while (it->isValid() && !isAborted()) {
    //         const ByteArray entry = it->key().byteArray();
    //         if (!string.isEmpty() && !entry.startsWith(string))
    //             break;
    //         if (!skipParentheses || !entry.contains('(')) {
    //             const Map<Location, bool> locations = it->value<Map<Location, bool> >();
    //             for (Map<Location, bool>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
    //                 if (!i->second && (!hasFilter || filter(i->first.path()))) {
    //                     if (elispList) {
    //                         write(entry);
    //                     } else {
    //                         out.append(entry);
    //                     }
    //                     break;
    //                 }
    //             }
    //         }
    //         it->next();
    //     }
    // }

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


