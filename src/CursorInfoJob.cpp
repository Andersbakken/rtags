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
    if (map.isEmpty())
        return;
    SymbolMap::const_iterator it = RTags::findCursorInfo(map, location);
    if (it != map.end()) {
        write(it->first);
        write(it->second);
    } else {
        it = map.lower_bound(location);
        if (it == map.end())
            --it;
    }
    if (it != map.begin()) {
        const uint32_t fileId = location.fileId();
        const int offset = location.offset();
        while (true) {
            --it;
            if (it->first.fileId() != fileId)
                break;
            if (RTags::isContainer(it->second.kind) && offset >= it->second.start && offset <= it->second.end) {
                write("Container:");
                write(it->first);
                write(it->second.toString(CursorInfo::IgnoreTargets|CursorInfo::IgnoreReferences, keyFlags()).constData());
            }
            if (it == map.begin())
                break;
        }
    }
}
