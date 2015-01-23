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
    const Symbol cursor = project()->findSymbol(location);
    if (cursor.isNull())
        return 1;

    const Location targetLocation = project()->findTarget(cursor);
    if (targetLocation.isNull())
        return 1;
    const Symbol target = project()->findSymbol(targetLocation);

    if (cursor.usr == target.usr) {
        write(target.location);
        return 0;
    }

    if (queryFlags() & QueryMessage::DeclarationOnly ? target.isDefinition() : !target.isDefinition()) {
        const Location otherLoc = project()->findTarget(target.location);
        if (!otherLoc.isNull()) {
            write(otherLoc);
            return 0;
        }
    }
    write(target.location);
    return 0;
}
