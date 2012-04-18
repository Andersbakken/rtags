#include "FollowLocationJob.h"
#include "Rdm.h"
#include "LevelDB.h"

FollowLocationJob::FollowLocationJob(int i, const RTags::Location &loc, unsigned f)
    : id(i), location(loc), flags(f)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::run()
{
    LevelDB db;
    if (!db.open(Server::Symbol, LevelDB::ReadOnly)) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db.db(), location);
    QList<QByteArray> list;
    if (!cursorInfo.target.isNull()) {
        list.append(cursorInfo.target.key(flags));
    }
    emit complete(id, list);
}
