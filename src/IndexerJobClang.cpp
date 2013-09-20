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
    : IndexerJob(id, type, project, sourceInformation), mState(Pending)
{
}

IndexerJobClang::IndexerJobClang(const QueryMessage &msg, const std::shared_ptr<Project> &project,
                                 const SourceInformation &sourceInformation)
    : IndexerJob(msg, project, sourceInformation), mState(Pending)
{
}

void IndexerJobClang::start()
{
    assert(project.lock());
    Server::instance()->processPool()->add(std::static_pointer_cast<IndexerJobClang>(shared_from_this()));
}

bool IndexerJobClang::abort()
{
    if (mState == Pending)
        return false;
    mState = Aborted;
    // ### should actually kill process maybe? If so, how about the visited files?
    return true;
}

bool IndexerJobClang::init(Path &path, List<String> &, String &data)
{
    assert(mState == Pending);
    mState = Running;
    std::shared_ptr<Project> proj = project.lock();
    static const Path rp = Rct::executablePath().parentDir() + "rp";
    Serializer serializer(data);
    const List<String> args = (sourceInformation.args
                               + CompilerManager::flags(sourceInformation.compiler)
                               + Server::instance()->options().defaultArguments);
    serializer << Server::instance()->options().socketFile << sourceInformation.sourceFile()
               << sourceInformation.fileId << proj->path() << args
               << static_cast<uint8_t>(type) << id;
    ::error() << "Running" << path << args;
    path = rp;
    return true;
}

void IndexerJobClang::error(const String &err)
{
    ::error() << "Got error trying to clang" << err << sourceInformation;
#warning Have to resubmit or something
}

void IndexerJobClang::finished(Process *process)
{
    ::error() << sourceInformation.sourceFile() << "finished" << process->returnCode();
    ::error() << process->readAllStdOut();
    ::error() << process->readAllStdErr();
}
