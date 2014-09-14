/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "FollowLocationJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "Project.h"

FollowLocationJob::FollowLocationJob(const Location &loc, const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project)
    : QueryJob(query, 0, project), location(loc)
{
}

int FollowLocationJob::execute()
{
    const SymbolMap &map = project()->symbols();
    SymbolMap::const_iterator it = RTags::findCursorInfo(map, location);

    if (it == map.end())
        return 1;

    const std::shared_ptr<CursorInfo> &cursorInfo = it->second;
    if (cursorInfo && cursorInfo->isClass() && cursorInfo->isDefinition()) {
        return 2;
    }

    SymbolMap targets;
    if (cursorInfo->kind == CXCursor_ObjCMessageExpr) {
        for (const auto &loc : cursorInfo->targets) {
            const auto target = map.value(loc);
            if (target)
                targets[loc] = target;

        }
    } else {
        Location loc;
        std::shared_ptr<CursorInfo> target = cursorInfo->bestTarget(map, &loc);
        if (!loc.isNull() && target)
            targets[loc] = target;
    }
    int ret = 1;
    for (const auto &t : targets) {
        auto target = t.second;
        Location loc = t.first;
        if (cursorInfo->kind != target->kind) {
            if (!target->isDefinition() && !target->targets.isEmpty()) {
                switch (target->kind) {
                case CXCursor_ClassDecl:
                case CXCursor_ClassTemplate:
                case CXCursor_StructDecl:
                case CXCursor_FunctionDecl:
                case CXCursor_CXXMethod:
                case CXCursor_Destructor:
                case CXCursor_Constructor:
                case CXCursor_FunctionTemplate:
                    target = target->bestTarget(map, &loc);
                    break;
                default:
                    break;
                }
            }
        }
        if (!loc.isNull()) {
            if (queryFlags() & QueryMessage::DeclarationOnly && target->isDefinition()) {
                Location declLoc;
                const std::shared_ptr<CursorInfo> decl = target->bestTarget(map, &declLoc);
                if (!declLoc.isNull()) {
                    write(declLoc);
                    ret = 0;
                }
            } else {
                write(loc);
                ret = 0;
            }
        }
    }
    return ret;
}
