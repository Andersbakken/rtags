#include "MatchJob.h"
#include "Database.h"
#include "Log.h"
#include "RTags.h"
#include "Rdm.h"
#include "LevelDB.h"

MatchJob::MatchJob(const QByteArray& p, int i, QueryMessage::Type t, bool ctx)
    : partial(p), id(i), type(t), includeContext(ctx)
{
}

void MatchJob::run()
{
    LevelDB db;
    if (!db.open(Database::SymbolName, LevelDB::ReadOnly)) {
        emit complete(id, QList<QByteArray>());
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
                    result.append(loc.key(includeContext ? RTags::Location::ShowContext : RTags::Location::NoFlag));
                }
            } else if (cmp > 0) {
                break;
            }
        }
        it->Next();
    }
#if 0
    if (result.isEmpty()) {
        it->SeekToFirst();
        while (it->Valid()) {
            const leveldb::Slice k = it->key();
            const QByteArray key = QByteArray::fromRawData(k.data(), k.size());
            debug() << key << partial;
            it->Next();
        }
    }
#endif
    delete it;
    emit complete(id, result);
}
