#include "Database.h"
#include "MatchJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"
#include "Rdm.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::WriteUnfiltered|Job::QuoteOutput : Job::WriteUnfiltered;
}

MatchJob::MatchJob(int i, const QueryMessage &query)
    : Job(i, i == CompletionMatchJobId ? CompletionMatchJobPriority : QueryJobPriority, jobFlags(query.flags())),
      partial(query.query().front()), type(query.type()),
      keyFlags(query.keyFlags()), queryFlags(query.flags())
{
    setPathFilters(query.pathFilters(), queryFlags & QueryMessage::FilterSystemIncludes);
}

void MatchJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::SymbolName, ScopedDB::Read);
    const bool hasFilter = !pathFilters().isEmpty();

    QByteArray entry;
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seek(partial.constData());
    const bool skipParentheses = queryFlags & QueryMessage::SkipParentheses;
    if (queryFlags & QueryMessage::ElispList)
        writeRaw("(list");
    while (it->isValid() && !isAborted()) {
        entry = it->key().byteArray();
        if (type == QueryMessage::ListSymbols) {
            if (partial.isEmpty() || entry.startsWith(partial)) {
                if (!skipParentheses || !entry.contains('(')) {
                    bool ok = true;
                    if (hasFilter) {
                        ok = false;
                        const QSet<Location> locations = it->value<QSet<Location> >();
                        foreach(const Location &loc, locations) {
                            if (filter(loc.path())) {
                                ok = true;
                                break;
                            }
                        }
                    }
                    if (ok)
                        write(entry);
                }
            } else {
                break;
            }
        } else {
            const int cmp = strcmp(partial.constData(), entry.constData());
            if (!cmp) {
                const QSet<Location> locations = it->value<QSet<Location> >();
                foreach (const Location &loc, locations) {
                    if (filter(loc.path()))
                        write(loc.key(keyFlags));
                }
            } else if (cmp > 0) {
                break;
            }
        }
        it->next();
    }
    if (queryFlags & QueryMessage::ElispList)
        writeRaw(")");
}
MatchJob * MatchJob::createCompletionMatchJob()
{
    QueryMessage msg(QueryMessage::ListSymbols);
    return new MatchJob(CompletionMatchJobId, msg);
}
