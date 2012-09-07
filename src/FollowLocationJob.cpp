#include "FollowLocationJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"

FollowLocationJob::FollowLocationJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &project)
    : Job(query, 0, project), location(loc)
{
}

void FollowLocationJob::execute()
{
    Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
    const SymbolMap &map = scope.data();
    const SymbolMap::const_iterator it = RTags::findCursorInfo(map, location);
    if (it == map.end())
        return;

    const CursorInfo &cursorInfo = it->second;
    if (cursorInfo.target.isNull())
        return;

    Location out = cursorInfo.target;
    if (RTags::isReference(cursorInfo.kind)) {
        SymbolMap::const_iterator target = RTags::findCursorInfo(map, cursorInfo.target);
        if (target != map.end() && !target->second.isDefinition) {
            switch (target->second.kind) {
            case CXCursor_FunctionDecl:
            case CXCursor_CXXMethod:
            case CXCursor_Destructor:
            case CXCursor_Constructor:
                target = RTags::findCursorInfo(map, target->second.target);
                if (target != map.end())
                    out = target->first;
                break;
            default:
                break;
            }
        }
    }
    assert(!out.isNull());
    write(out.key(keyFlags()));
}
