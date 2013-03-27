#include "CursorInfoJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "Project.h"

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
    SymbolMap::const_iterator it = RTags::findCursorInfo(map, location, context());

    unsigned ciFlags = 0;
    if (!(queryFlags() & QueryMessage::CursorInfoIncludeTargets))
        ciFlags |= CursorInfo::IgnoreTargets;
    if (!(queryFlags() & QueryMessage::CursorInfoIncludeReferences))
        ciFlags |= CursorInfo::IgnoreReferences;
    if (it != map.end()) {
        write(it->first);
        write(it->second, ciFlags);
    } else {
        it = map.lower_bound(location);
        if (it == map.end())
            --it;
    }
    ciFlags |= CursorInfo::IgnoreTargets|CursorInfo::IgnoreReferences;
    if (it != map.begin() && queryFlags() & QueryMessage::CursorInfoIncludeParents) {
        const uint32_t fileId = location.fileId();
        const int offset = location.offset();
        while (true) {
            --it;
            if (it->first.fileId() != fileId)
                break;
            if (it->second.isDefinition() && RTags::isContainer(it->second.kind) && offset >= it->second.start && offset <= it->second.end) {
                write("====================");
                write(it->first);
                write(it->second, ciFlags);
            }
            if (it == map.begin())
                break;
        }
    }
}
