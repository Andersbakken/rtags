#include "MatchJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"
#include "Rdm.h"
#include "LevelDB.h"

MatchJob::MatchJob(const QByteArray& p, int i, QueryMessage::Type t, unsigned flags)
    : Job(i), partial(p), type(t), keyFlags(flags)
{
}

void MatchJob::run()
{
    LevelDB db;
    if (!db.open(Server::SymbolName, LevelDB::ReadOnly)) {
        finish();
        return;
    }


    QList<QByteArray> result;

    QByteArray entry;
    leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
    it->Seek(partial.constData());
    while (it->Valid()) {
        entry = it->key().ToString().c_str();
        /*if ((entry.contains('(') && !partial.contains('('))
          || (entry.contains(':') && !partial.contains(':'))) {
          it->Next();
          continue;
          }*/
        if (type == QueryMessage::ListSymbols) {
            if (entry.startsWith(partial)) {
                result.append(entry);
            } else {
                break;
            }
        } else {
            const int cmp = strcmp(partial.constData(), entry.constData());
            if (!cmp) {
                const QSet<RTags::Location> locations = Rdm::readValue<QSet<RTags::Location> >(it);
                foreach (const RTags::Location &loc, locations) {
                    result.append(loc.key(keyFlags));
                }
            } else if (cmp > 0) {
                break;
            }
        }
        it->Next();
    }
    delete it;
    finish(result);
}
