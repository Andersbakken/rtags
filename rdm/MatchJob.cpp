#include "MatchJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"
#include "Rdm.h"

MatchJob::MatchJob(int i, const QueryMessage &query)
    : Job(i, WriteUnfiltered), partial(query.query().front()), type(query.type()),
      keyFlags(query.keyFlags()), skipParentheses(query.flags() & QueryMessage::SkipParentheses)
{
    setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
}

void MatchJob::execute()
{
    Database *db = Server::instance()->db(Server::SymbolName);
    const bool hasFilter = !pathFilters().isEmpty();

    QByteArray entry;
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seek(partial.constData());
    while (it->isValid() && !isAborted()) {
        entry = QByteArray(it->key().data(), it->key().size());
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
}
MatchJob * MatchJob::createCompletionMatchJob()
{
    QueryMessage msg(QueryMessage::ListSymbols);
    return new MatchJob(CompletionMatchJobId, msg);
}
