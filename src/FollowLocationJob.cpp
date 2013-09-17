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

FollowLocationJob::FollowLocationJob(const Location &loc, const QueryMessage &query, const std::shared_ptr<Project> &project)
    : Job(query, 0, project), location(loc)
{
}

void FollowLocationJob::execute()
{
    const SymbolMap &map = project()->symbols();
    const ErrorSymbolMap &errorSymbols = project()->errorSymbols();

    const ErrorSymbolMap::const_iterator e = errorSymbols.find(location.fileId());
    const SymbolMap *errors = e == errorSymbols.end() ? 0 : &e->second;

    bool foundInError = false;
    SymbolMap::const_iterator it = RTags::findCursorInfo(map, location, context(), errors, &foundInError);

    if (it == map.end())
        return;

    const CursorInfo &cursorInfo = it->second;
    if (cursorInfo.isClass() && cursorInfo.isDefinition()) {
        return;
    }

    Location loc;
    CursorInfo target = cursorInfo.bestTarget(map, errors, &loc);
    if (target.isNull() && foundInError) {
        target = cursorInfo.bestTarget(e->second, errors, &loc);
    }
    if (!loc.isNull()) {
        // ### not respecting DeclarationOnly
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
                    target = target.bestTarget(map, errors, &loc);
                    if (target.isNull() && foundInError)
                        target = cursorInfo.bestTarget(e->second, errors, &loc);

                    break;
                default:
                    break;
                }
            }
        }
        if (!loc.isNull()) {
            if (queryFlags() & QueryMessage::DeclarationOnly && target.isDefinition()) {
                Location declLoc;
                const CursorInfo decl = target.bestTarget(map, errors, &declLoc);
                if (!declLoc.isNull()) {
                    write(declLoc);
                }
            } else {
                write(loc);
            }
        }
    }
}
