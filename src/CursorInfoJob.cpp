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

#include "CursorInfoJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "Project.h"
#include "QueryMessage.h"

CursorInfoJob::CursorInfoJob(const Location &loc, const QueryMessage &query, const std::shared_ptr<Project> &proj)
    : Job(query, 0, proj), location(loc)
{
}

void CursorInfoJob::execute()
{
    const SymbolMap &map = project()->symbols();
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
