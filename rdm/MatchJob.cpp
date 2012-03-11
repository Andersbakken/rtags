#include "MatchJob.h"
#include "Database.h"
#include <leveldb/db.h>

MatchJob::MatchJob(const QByteArray& p, int i)
    : partial(p), id(i)
{
}

MatchJob::~MatchJob()
{
}

void MatchJob::run()
{
    QByteArray databasename = Database::databaseName(Database::Symbol);

    leveldb::DB* db = 0;
    leveldb::Status status = leveldb::DB::Open(leveldb::Options(), databasename.constData(), &db);
    if (!status.ok()) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    QList<QByteArray> result;

    QByteArray entry;
    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    it->Seek(partial.constData());
    while (it->Valid()) {
        entry = it->key().ToString().c_str();
        /*if ((entry.contains('(') && !partial.contains('('))
            || (entry.contains(':') && !partial.contains(':'))) {
            it->Next();
            continue;
        }*/
        if (entry.startsWith(partial))
            result.append(entry);
        else
            break;
        it->Next();
    }
    delete it;

    delete db;

    emit complete(id, result);
}
