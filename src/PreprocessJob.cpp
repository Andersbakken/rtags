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
#include "Cpp.h"

PreprocessJob::PreprocessJob(Source &&source, const std::shared_ptr<Project> &project, IndexerJob::IndexType type)
    : mSource(std::forward<Source>(source)), mProject(project), mType(type)
{
}

void PreprocessJob::run()
{
    switch (mSource.language) {
    case Source::C:
    case Source::CPlusPlus:
    case Source::CPlusPlus11: {
        std::shared_ptr<Cpp> cpp = RTags::preprocess(mSource, mProject);
        if (!cpp) {
            error() << "Couldn't preprocess" << mSource.sourceFile();
            return;
        }

        Source source = std::move(mSource);
        const IndexerJob::IndexType type = mType;
        std::shared_ptr<Project> project = std::move(mProject);
        EventLoop::mainEventLoop()->callLater([source, cpp, project, type]() {
                Server::instance()->index(source, cpp, project, type);
            });
        break; }
    default:
        assert(0);
        break;
    }
}
