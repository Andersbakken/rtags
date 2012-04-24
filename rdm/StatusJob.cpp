#include "StatusJob.h"
#include "Server.h"
#include <clang-c/Index.h>
#include <Rdm.h>
#include "LevelDB.h"

StatusJob::StatusJob(int i, const QByteArray &q)
    : id(i), query(q)
{
}

void StatusJob::run()
{
    QList<QByteArray> ret;
    if (query.isEmpty() || query == "general") {
        LevelDB db;
        if (db.open(Server::General, LevelDB::ReadOnly)) {
            ret.append(Server::databaseName(Server::General));
            ret.append("    version: " + QByteArray::number(Rdm::readValue<int>(db.db(), "version")));
        }
    }

    if (query.isEmpty() || query == "dependencies") {
        LevelDB db;
        if (db.open(Server::Dependency, LevelDB::ReadOnly)) {
            ret.append(Server::databaseName(Server::Dependency));
            leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
            it->SeekToFirst();
            char buf[1024];
            memcpy(buf, "  ", 2);
            while (it->Valid()) {
                memcpy(buf + 2, it->key().data(), it->key().size());
                memcpy(buf + 2 + it->key().size(), " is depended on by:", 20);
                ret.append(buf);
                const QSet<Path> deps = Rdm::readValue<QSet<Path> >(it);
                memcpy(buf + 2, "  ", 2);
                foreach (const Path &p, deps) {
                    memcpy(buf + 4, p.constData(), p.size() + 1);
                    ret.append(buf);
                }
                it->Next();
            }
        }
    }

    if (query.isEmpty() || query == "symbols") {
    }

    if (query.isEmpty() || query == "symbolnames") {
    }

    if (query.isEmpty() || query == "fileinfos") {
    }

    if (query.isEmpty() || query == "pch") {
    }
    emit complete(id, ret);
}
