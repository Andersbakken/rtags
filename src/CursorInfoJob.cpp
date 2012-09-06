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
    Scope<const SymbolMap &> scope = project()->lockSymbolsForRead();
    const SymbolMap &map = scope.data();
    const SymbolMap::const_iterator it = RTags::findCursorInfo(map, location);
    if (it != map.end())
        write(it->first, it->second);
}
