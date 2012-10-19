#include "CursorInfoJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, 0, proj), location(loc)
{
}

void CursorInfoJob::execute()
{
    Scope<const SymbolMap &> scope = project()->lockSymbolsForRead();
    if (scope.isNull())
        return;
    const SymbolMap &map = scope.data();
    const SymbolMap::const_iterator it = RTags::findCursorInfo(map, location);
    if (it != map.end()) {
        write(it->first);
        write(it->second);
    }
}
