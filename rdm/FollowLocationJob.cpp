#include "FollowLocationJob.h"
#include "Rdm.h"
#include "leveldb/db.h"

FollowLocationJob::FollowLocationJob(int i, const RTags::Location &loc, unsigned f)
    : Job(i), location(loc), flags(f)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::run()
{
    leveldb::DB *db = Server::instance()->db(Server::Symbol);
    Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db, location);
    if (!cursorInfo.target.isNull()) {
        write(cursorInfo.target.key(flags));
    }
    finish();
}
