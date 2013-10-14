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
#include <rct/Process.h>

#include "RTagsPlugin.h"

class ClangPlugin : public RTagsPlugin
{
public:
    virtual std::shared_ptr<IndexerJob> createJob(IndexType type,
                                                  const std::shared_ptr<Project> &project,
                                                  const Source &source)
    {
        if (source.language != Source::JavaScript)
            return std::shared_ptr<IndexerJob>(new IndexerJobClang(type, project, source));
        return std::shared_ptr<IndexerJob>();
    }
    virtual std::shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                                  const std::shared_ptr<Project> &project,
                                                  const Source &source,
                                                  Connection *conn)
    {
        if (source.language != Source::JavaScript)
            return std::shared_ptr<IndexerJob>(new IndexerJobClang(msg, project, source, conn));
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
                                 const Source &source)
    : IndexerJob(type, project, source), mState(Pending), mWaiting(0), mProcess(0)
{
}

IndexerJobClang::IndexerJobClang(const QueryMessage &msg, const std::shared_ptr<Project> &project,
                                 const Source &source, Connection *conn)
    : IndexerJob(msg, project, source, conn), mState(Pending), mWaiting(0)
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
        error() << "Project disappeared" << source;
        return 0;
    }
    const String preprocessed = RTags::preprocess(source);

    if (preprocessed.isEmpty()) {
        error() << "Couldn't preprocess" << source.sourceFile();
        return 0;
    }

    static const Path rp = Rct::executablePath().parentDir() + "rp";
    String stdinData;
    Serializer serializer(stdinData);
    serializer << Server::instance()->options().socketFile << source.sourceFile()
               << source.fileId << preprocessed << source.arguments
               << proj->path() << static_cast<uint8_t>(type);

    mProcess = new Process;
    if (!mProcess->start(rp)) {
        error() << "Couldn't start rp" << mProcess->errorString();
        delete mProcess;
        mProcess = 0;
        std::shared_ptr<IndexData> data(new IndexData(type));
        data->fileId = source.fileId;
        data->aborted = true;
        proj->onJobFinished(data);
        proj->dirty(source.sourceFile());
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
            data->fileId = source.fileId;
            data->aborted = true;
            proj->onJobFinished(data);
            // proj->dirty(source.sourceFile());
        }
    }
    // ::error() << source.sourceFile() << "finished" << process->returnCode() << mWaiting << mTimer.elapsed() << "ms";
}
