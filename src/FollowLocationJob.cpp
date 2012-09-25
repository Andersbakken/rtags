#include "FollowLocationJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"

FollowLocationJob::FollowLocationJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &project)
    : Job(query, 0, project), location(loc)
{
}

void FollowLocationJob::run()
{
    Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
    const SymbolMap &map = scope.data();
    const SymbolMap::const_iterator it = RTags::findCursorInfo(map, location);
    if (it == map.end())
        return;

    const CursorInfo &cursorInfo = it->second;

    Location loc;
    CursorInfo target = cursorInfo.bestTarget(map, &loc);
    if (!target.isNull()) {
        if (RTags::isReference(cursorInfo.kind)) {
            if (!target.isDefinition && !target.targets.isEmpty()) {
                switch (target.kind) {
                case CXCursor_FunctionDecl:
                case CXCursor_CXXMethod:
                case CXCursor_Destructor:
                case CXCursor_Constructor:
                    target = target.bestTarget(map, &loc);
                    break;
                default:
                    break;
                }
            }
        }
        if (!loc.isNull()) {
            write(loc.key(keyFlags()));
        }
    }
}
