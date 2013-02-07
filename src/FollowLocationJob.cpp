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
    if (scope.isNull())
        return;

    const SymbolMap &map = scope.data();
    bool moved;
    const SymbolMap::const_iterator it = RTags::findCursorInfo(map, location, &moved);
    if (moved) {
        write("Symbol has moved");
        return;
    }
    
    if (it == map.end())
        return;

    const CursorInfo &cursorInfo = it->second;
    if (cursorInfo.isClass() && cursorInfo.isDefinition())
        return;

    Location loc;
    CursorInfo target = cursorInfo.bestTarget(map, &loc);
    if (!loc.isNull()) {
        if (cursorInfo.kind != target.kind) {
            if (!target.isDefinition() && !target.targets.isEmpty()) {
                switch (target.kind) {
                case CXCursor_ClassDecl:
                case CXCursor_ClassTemplate:
                case CXCursor_StructDecl:
                case CXCursor_FunctionDecl:
                case CXCursor_CXXMethod:
                case CXCursor_Destructor:
                case CXCursor_Constructor:
                case CXCursor_FunctionTemplate:
                    target = target.bestTarget(map, &loc);
                    break;
                default:
                    break;
                }
            }
        }
        if (!loc.isNull())
            write(loc);
    }
}
