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
