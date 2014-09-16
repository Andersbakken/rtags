#include "JobScheduler.h"
#include "Project.h"
#include "Server.h"

JobScheduler::JobScheduler(int maxJobs)
    : mMaxJobs(maxJobs)
{}

JobScheduler::~JobScheduler()
{
    mPendingJobs.deleteAll();
    if (!mActiveByProcess.isEmpty()) {
        for (const auto &job : mActiveByProcess) {
            job.first->kill();
        }
    }
}

void JobScheduler::add(const std::shared_ptr<IndexerJob> &job)
{
    assert(!(job->flags & ~IndexerJob::Type_Mask));
    std::shared_ptr<Node> node(new Node({ job, 0 }));
    node->job = job;
    if (job->flags & IndexerJob::Dirty) {
        mPendingJobs.prepend(node);
    } else {
        mPendingJobs.append(node);
    }
    startJobs();
}

void JobScheduler::startJobs()
{
    static Path rp;
    if (rp.isEmpty()) {
        rp = Rct::executablePath().parentDir() + "rp";
        if (!rp.isFile()) {
            rp = Rct::executablePath();
            rp.resolve();
            rp = rp.parentDir() + "rp";
            if (!rp.isFile()) // should be in $PATH
                rp = "rp";
        }
    }

    while (mActiveByProcess.size() < mMaxJobs && !mPendingJobs.isEmpty()) {
        std::shared_ptr<Node> node = mPendingJobs.takeFirst();
        assert(node);
        assert(node->job);
        assert(!(node->job->flags & (IndexerJob::Running|IndexerJob::Complete|IndexerJob::Crashed)));
        if (node->job->flags & IndexerJob::Aborted) {
            debug() << node->job->sourceFile << "was aborted, discarding";
            continue;
        }

        Process *process = new Process;
        List<String> arguments;
        for (int i=logLevel(); i>0; --i)
            arguments << "-v";
        if (!process->start(rp, arguments)) {
            error() << "Couldn't start rp" << rp << process->errorString();
            delete process;
            node->job->flags |= IndexerJob::Crashed;
            jobFinished(node->job, createData(node->job));
            rp.clear(); // in case rp was missing for a moment and we fell back to searching $PATH
            continue;
        }
        process->finished().connect([this](Process *proc) {
                EventLoop::deleteLater(proc);
                auto node = mActiveByProcess.take(proc);
                assert(!node || node->process == proc);
                const String stdErr = proc->readAllStdErr();
                const String stdOut = proc->readAllStdOut();
                if (!stdOut.isEmpty() || !stdErr.isEmpty()) {
                    error() << (node ? node->job->sourceFile : String("Orphaned process")) << stdErr << stdOut;
                }

                if (node) {
                    assert(node->process == proc);
                    node->process = 0;
                    assert(!(node->job->flags & IndexerJob::Aborted));
                    if (!(node->job->flags & IndexerJob::Complete) && proc->returnCode() != 0) {
                        auto nodeById = mActiveById.take(node->job->id);
                        assert(nodeById);
                        assert(nodeById == node);
                        // job failed, probably no IndexerMessage coming
                        node->job->flags |= IndexerJob::Crashed;
                        jobFinished(node->job, createData(node->job));
                    }
                }
                startJobs();
            });


        node->process = process;
        assert(!(node->job->flags & ~IndexerJob::Type_Mask));
        node->job->flags |= IndexerJob::Running;
        process->write(node->job->encode());
        mActiveByProcess[process] = node;
        mActiveById[node->job->id] = node;
    }
}

void JobScheduler::handleIndexerMessage(const std::shared_ptr<IndexerMessage> &message)
{
    std::shared_ptr<IndexData> data = message->data();
    auto node = mActiveById.take(data->id);
    if (!node) {
        debug() << "Got IndexerMessage for unknown job";
        return;
    }
    jobFinished(node->job, data);
}

void JobScheduler::jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexData> &data)
{
    assert(!(job->flags & IndexerJob::Aborted));
    assert(job);
    assert(data);
    std::shared_ptr<Project> project = Server::instance()->project(job->project);
    if (!project)
        return;

    job->flags &= ~IndexerJob::Running;
    if (!(job->flags & IndexerJob::Crashed)) {
        job->flags |= IndexerJob::Complete;
    } else {
        ++job->crashCount;
        const auto &options = Server::instance()->options();
        assert(job->crashCount <= options.maxCrashCount);
        if (job->crashCount < options.maxCrashCount) {
            project->releaseFileIds(job->visited);
            job->flags &= ~IndexerJob::Crashed;
            EventLoop::eventLoop()->registerTimer([job, this](int) { add(job); }, 500, Timer::SingleShot); // give it 500 ms before we try again
            return;
        }
    }
    project->onJobFinished(job, data);
}

void JobScheduler::dump(Connection *conn)
{
    if (!mPendingJobs.isEmpty()) {
        conn->write("Pending:");
        for (const auto &node : mPendingJobs) {
            conn->write<128>("%s: 0x%x %s",
                             node->job->sourceFile.constData(),
                             node->job->flags,
                             IndexerJob::dumpFlags(node->job->flags).constData());
        }
    }
    if (!mActiveById.isEmpty()) {
        conn->write("Active:");
        for (const auto &node : mActiveById) {
            conn->write<128>("%s: 0x%x %s",
                             node.second->job->sourceFile.constData(),
                             node.second->job->flags,
                             IndexerJob::dumpFlags(node.second->job->flags).constData());
        }
    }
}

void JobScheduler::abort(const std::shared_ptr<IndexerJob> &job)
{
    job->flags |= IndexerJob::Aborted;
    job->flags &= ~IndexerJob::Running;
    auto node = mActiveById.take(job->id);
    if (!node) {// if it's pending we will just not start it when it's ready due to the Aborted flag
        // error() << "aborting" << job->sourceFile << "no node";
        return;
    }
    if (node->process) {
        // error() << "Killing process" << node->process;
        node->process->kill();
        mActiveByProcess.remove(node->process);
    }
}
std::shared_ptr<IndexData> JobScheduler::createData(const std::shared_ptr<IndexerJob> &job)
{
    std::shared_ptr<IndexData> data(new IndexData(job->flags));
    data->key = job->source.key();
    data->dependencies[job->source.fileId].insert(job->source.fileId);
    return data;

}
