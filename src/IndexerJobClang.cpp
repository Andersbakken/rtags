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

#include "IndexerJobClang.h"
#include "Project.h"
#include "Server.h"
#include "CompilerManager.h"
#include <rct/Process.h>

#include "RTagsPlugin.h"

class ClangPlugin : public RTagsPlugin
{
public:
    virtual std::shared_ptr<IndexerJob> createJob(uint64_t id,
                                                  IndexType type,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation)
    {
        if (!sourceInformation.isJS())
            return std::shared_ptr<IndexerJob>(new IndexerJobClang(id, type, project, sourceInformation));
        return std::shared_ptr<IndexerJob>();
    }
    virtual std::shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                             const std::shared_ptr<Project> &project,
                                             const SourceInformation &sourceInformation)
    {
        if (!sourceInformation.isJS())
            return std::shared_ptr<IndexerJob>(new IndexerJobClang(msg, project, sourceInformation));
        return std::shared_ptr<IndexerJob>();
    }
};

extern "C" {
RTagsPlugin *createInstance()
{
    return new ClangPlugin;
}
};

IndexerJobClang::IndexerJobClang(uint64_t id, IndexType type, const std::shared_ptr<Project> &project,
                                 const SourceInformation &sourceInformation)
    : IndexerJob(id, type, project, sourceInformation)
{
}

IndexerJobClang::IndexerJobClang(const QueryMessage &msg, const std::shared_ptr<Project> &project,
                                 const SourceInformation &sourceInformation)
    : IndexerJob(msg, project, sourceInformation)
{
}

void IndexerJobClang::start()
{
    assert(project.lock());
    Server::instance()->processPool()->add(project.lock(), sourceInformation.fileId, type);
}

// ### we really should be able to return false for aborts that happened before
// ### the process was started, need a callback from ProcessPool for this
bool IndexerJobClang::abort()
{
    Server::instance()->processPool()->cancel(sourceInformation.fileId);
    return true;
}
