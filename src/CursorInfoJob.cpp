#include "CursorInfoJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(int i, const Location &loc, unsigned queryFlags)
    : Job(i, 0, queryFlags), location(loc)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

void CursorInfoJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Read);
    Location found;
    const CursorInfo ci = RTags::findCursorInfo(db, location, &found);
    if (isAborted())
        return;
    write(found, ci);
}
