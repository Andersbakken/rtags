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
    virtual std::shared_ptr<IndexerJob> createJob(IndexType type,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation)
    {
        if (!sourceInformation.isJS())
            return std::shared_ptr<IndexerJob>(new IndexerJobClang(type, project, sourceInformation));
        return std::shared_ptr<IndexerJob>();
    }
    virtual std::shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                                  const std::shared_ptr<Project> &project,
                                                  const SourceInformation &sourceInformation,
                                                  Connection *conn)
    {
        if (!sourceInformation.isJS())
            return std::shared_ptr<IndexerJob>(new IndexerJobClang(msg, project, sourceInformation, conn));
        return std::shared_ptr<IndexerJob>();
    }
};

extern "C" {
RTagsPlugin *createInstance()
{
    return new ClangPlugin;
}
};

IndexerJobClang::IndexerJobClang(IndexType type, const std::shared_ptr<Project> &project,
                                 const SourceInformation &sourceInformation)
    : IndexerJob(type, project, sourceInformation), mState(Pending), mWaiting(0), mProcess(0)
{
}

IndexerJobClang::IndexerJobClang(const QueryMessage &msg, const std::shared_ptr<Project> &project,
                                 const SourceInformation &sourceInformation, Connection *conn)
    : IndexerJob(msg, project, sourceInformation, conn), mState(Pending), mWaiting(0)
{
}

void IndexerJobClang::start()
{
    assert(project.lock());
    Server::instance()->processPool()->add(std::static_pointer_cast<IndexerJobClang>(shared_from_this()));
}

bool IndexerJobClang::abort()
{
    if (mState == Pending) {
        assert(!mProcess);
        return false;
    }
    if (mProcess) {
        mProcess->stop();
        mProcess = 0;
    }
    mState = Aborted;
    return true;
}

Process *IndexerJobClang::startProcess()
{
    mWaiting = mTimer.elapsed();
    assert(mState == Pending);
    mState = Running;
    std::shared_ptr<Project> proj = project.lock();
    if (!proj) {
        error() << "Project disappeared" << sourceInformation;
        return 0;
    }
    static const Path rp = Rct::executablePath().parentDir() + "rp";
    String stdinData;
    Serializer serializer(stdinData);
    const List<String> args = (sourceInformation.args
                               + CompilerManager::flags(sourceInformation.compiler)
                               + Server::instance()->options().defaultArguments);
    serializer << Server::instance()->options().socketFile << sourceInformation.sourceFile()
               << sourceInformation.fileId << proj->path() << args
               << static_cast<uint8_t>(type);

    mProcess = new Process;
    if (!mProcess->start(rp)) {
        error() << "Couldn't start rp" << mProcess->errorString();
        delete mProcess;
        mProcess = 0;
        std::shared_ptr<IndexData> data(new IndexData(type));
        data->fileId = sourceInformation.fileId;
        data->aborted = true;
        proj->onJobFinished(data);
        proj->dirty(sourceInformation.sourceFile());
        return 0;
    }
    mProcess->write(stdinData);
    return mProcess;
}

void IndexerJobClang::finished(Process *process)
{
    ::error() << process->readAllStdOut();
    ::error() << process->readAllStdErr();
    if (process->returnCode() == -1) {
        std::shared_ptr<Project> proj = project.lock();
        if (proj) {
            std::shared_ptr<IndexData> data(new IndexData(type));
            data->fileId = sourceInformation.fileId;
            data->aborted = true;
            proj->onJobFinished(data);
            // proj->dirty(sourceInformation.sourceFile());
        }
    }
    // ::error() << sourceInformation.sourceFile() << "finished" << process->returnCode() << mWaiting << mTimer.elapsed() << "ms";
}
