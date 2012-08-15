#include "CursorInfoJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, 0, proj), location(loc)
{
}

void CursorInfoJob::execute()
{
    ScopedDB database = db(Project::Symbol, ReadWriteLock::Read);
    Location found;
    const CursorInfo ci = RTags::findCursorInfo(database, location, &found);
    if (isAborted())
        return;
    write(found, ci);
}
