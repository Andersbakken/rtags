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
#include "Project.h"

FollowLocationJob::FollowLocationJob(const Location &loc,
                                     const std::shared_ptr<QueryMessage> &query,
                                     const std::shared_ptr<Project> &project)
    : QueryJob(query, 0, project), location(loc)
{
}

int FollowLocationJob::execute()
{
    Location loc = project()->findTarget(location);
    int ret = 1;
    if (!loc.isNull()) {
        auto target = project()->findCursor(loc);
        if (queryFlags() & QueryMessage::DeclarationOnly && target.isDefinition()) {
            const Location declLoc = project()->findTarget(loc);
            if (!declLoc.isNull()) {
                write(declLoc);
                ret = 0;
            }
        } else {
            write(loc);
            ret = 0;
        }
    }
    return ret;
}
