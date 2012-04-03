#include "FollowLocationJob.h"
#include "Rdm.h"
#include "LevelDB.h"

FollowLocationJob::FollowLocationJob(int i, const RTags::Location &loc, bool ctx)
    : id(i), location(loc), includeContext(ctx)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::run()
{
    LevelDB db;
    if (!db.open(Database::Symbol, LevelDB::ReadOnly)) {
        emit complete(id, QList<QByteArray>());
        return;
    }

    Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db.db(), location);
    QList<QByteArray> list;
    if (!cursorInfo.target.isNull()) {
        list.append(cursorInfo.target.key(includeContext
                                          ? RTags::Location::ShowContext
                                          : RTags::Location::NoFlag));
    }
    emit complete(id, list);
}
