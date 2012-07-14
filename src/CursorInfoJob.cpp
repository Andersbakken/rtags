#include "CursorInfoJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(int i, const Location &loc, unsigned f)
    : Job(i), location(loc), flags(f & ~Location::ShowContext)
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
