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

#include "PreprocessJob.h"
#include "RTagsClang.h"
#include "Server.h"
#include "Unit.h"

PreprocessJob::PreprocessJob(Source &&source, const std::shared_ptr<Project> &project, uint32_t flags)
    : mSource(std::forward<Source>(source)), mProject(project), mFlags(flags)
{
}

void PreprocessJob::run()
{
    std::shared_ptr<Unit> unit = RTags::preprocess(mSource, mProject, mFlags);
    if (!unit) {
        error() << "Couldn't preprocess" << mSource.sourceFile();
        return;
    }

    std::shared_ptr<Project> project = std::move(mProject);
    EventLoop::mainEventLoop()->callLater([unit, project]() {
            Server::instance()->index(unit, project);
        });
}
