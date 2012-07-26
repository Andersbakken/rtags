#include "CursorInfoJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(const Location &loc, const QueryMessage &query)
    : Job(query, 0), location(loc)
{
}

void CursorInfoJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, Server::Read);
    Location found;
    const CursorInfo ci = RTags::findCursorInfo(db, location, &found);
    if (isAborted())
        return;
    write(found, ci);
}
