#include "JobScheduler.h"
#include "Project.h"
#include "Server.h"

JobScheduler::JobScheduler()
    : mProcrastination(0)
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
    std::shared_ptr<Node> node(new Node({ job, 0, 0, 0 }));
    node->job = job;
    // error() << job->priority << job->sourceFile << mProcrastination;
    if (mPendingJobs.isEmpty() || job->priority > mPendingJobs.first()->job->priority) {
        mPendingJobs.prepend(node);
    } else {
        std::shared_ptr<Node> after = mPendingJobs.last();
        while (job->priority > after->job->priority) {
            after = after->prev;
            assert(after);
        }
        mPendingJobs.insert(node, after);
    }
    assert(!mInactiveById.contains(job->id));
    mInactiveById[job->id] = node;
    // error() << "procrash" << mProcrastination << job->sourceFile;
    if (!mProcrastination)
        startJobs();
}

uint32_t JobScheduler::hasHeaderError(DependencyNode *node, Set<uint32_t> &seen) const
{
    assert(node);
    for (auto dep : node->includes) {
        if (!seen.insert(dep.first))
            continue;

        if (mHeaderErrors.contains(dep.first)) {
            return dep.first;
        }
        if (uint32_t fileId = hasHeaderError(dep.second, seen)) {
            return fileId;
        }
    }
    return 0;
}

uint32_t JobScheduler::hasHeaderError(uint32_t file, const std::shared_ptr<Project> &project) const
{
    DependencyNode *node = project->dependencies().value(file);
    if (!node)
        return false;
    Set<uint32_t> seen;
    return hasHeaderError(node, seen);
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

    const auto &options = Server::instance()->options();
    std::shared_ptr<Node> node = mPendingJobs.first();
    auto cont = [&node, this]() {
        auto tmp = node->next;
        mPendingJobs.remove(node);
        node = tmp;
    };

    while (mActiveByProcess.size() < options.jobCount && node) {
        assert(node);
        assert(node->job);
        assert(!(node->job->flags & (IndexerJob::Running|IndexerJob::Complete|IndexerJob::Crashed|IndexerJob::Aborted)));
        std::shared_ptr<Project> project = Server::instance()->project(node->job->project);
        if (!project) {
            cont();
            debug() << node->job->sourceFile << "doesn't have a project, discarding";
            continue;
        }

        uint32_t headerError = 0;
        if (!mHeaderErrors.isEmpty()) {
            headerError = hasHeaderError(node->job->source.fileId, project);
            if (headerError) {
                // error() << "We got a headerError" << Location::path(headerError) << "for" << node->job->source.sourceFile()
                //         << mHeaderErrorMaxJobs << mHeaderErrorJobIds;
                if (options.headerErrorJobCount <= mHeaderErrorJobIds.size()) {
                    warning() << "Holding off on" << node->job->sourceFile << "it's got a header error from" << Location::path(headerError);
                    node = node->next;
                    continue;
                }
            }
        }

        Process *process = new Process;
        List<String> arguments;
        for (int i=logLevel(); i>0; --i)
            arguments << "-v";
        if (!process->start(rp, arguments)) {
            error() << "Couldn't start rp" << rp << process->errorString();
            delete process;
            node->job->flags |= IndexerJob::Crashed;
            jobFinished(node->job, std::shared_ptr<IndexDataMessage>(new IndexDataMessage(node->job)));
            rp.clear(); // in case rp was missing for a moment and we fell back to searching $PATH
            cont();
            continue;
        }
        if (headerError) {
            node->job->priority = IndexerJob::HeaderError;
            warning() << "Letting" << node->job->sourceFile << "go even with a headerheader error from" << Location::path(headerError);
            mHeaderErrorJobIds.insert(node->job->id);
        }
        const uint64_t jobId = node->job->id;
        process->finished().connect([this, jobId](Process *proc) {
                EventLoop::deleteLater(proc);
                auto node = mActiveByProcess.take(proc);
                assert(!node || node->process == proc);
                const String stdErr = proc->readAllStdErr();
                const String stdOut = proc->readAllStdOut();
                if (!stdOut.isEmpty() || !stdErr.isEmpty()) {
                    error() << (node ? node->job->sourceFile : String("Orphaned process")) << '\n' << stdErr << stdOut;
                }

                if (node) {
                    assert(node->process == proc);
                    node->process = 0;
                    assert(!(node->job->flags & IndexerJob::Aborted));
                    if (!(node->job->flags & IndexerJob::Complete) && proc->returnCode() != 0) {
                        auto nodeById = mActiveById.take(jobId);
                        assert(nodeById);
                        assert(nodeById == node);
                        // job failed, probably no IndexDataMessage coming
                        node->job->flags |= IndexerJob::Crashed;
                        jobFinished(node->job, std::shared_ptr<IndexDataMessage>(new IndexDataMessage(node->job)));
                    }
                }
                mHeaderErrorJobIds.remove(jobId);
                startJobs();
            });


        node->process = process;
        assert(!(node->job->flags & ~IndexerJob::Type_Mask));
        node->job->flags |= IndexerJob::Running;
        process->write(node->job->encode());
        mActiveByProcess[process] = node;
        mInactiveById.remove(jobId);
        mActiveById[jobId] = node;
        cont();
    }
}

void JobScheduler::handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message)
{
    auto node = mActiveById.take(message->id());
    if (!node) {
        debug() << "Got IndexDataMessage for unknown job";
        return;
    }
    jobFinished(node->job, message);
}

void JobScheduler::jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message)
{
    mHeaderErrors.unite(message->headerErrors());
    assert(!(job->flags & IndexerJob::Aborted));
    assert(job);
    assert(message);
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
            EventLoop::eventLoop()->registerTimer([job, this](int) {
                    if (!(job->flags & IndexerJob::Aborted)) {
                        job->flags &= ~IndexerJob::Crashed;
                        job->acquireId();
                        add(job);
                    }
                }, 500, Timer::SingleShot); // give it 500 ms before we try again
            return;
        }
    }
    project->onJobFinished(job, message);
}

void JobScheduler::dump(const std::shared_ptr<Connection> &conn)
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

    if (!mHeaderErrorJobIds.isEmpty()) {
        conn->write("HeaderErrorJobs:");
        for (uint64_t headerErrorJobId : mHeaderErrorJobIds) {
            auto node = mActiveById.value(headerErrorJobId);
            assert(node);
            conn->write<128>("%s: 0x%x %s",
                             node->job->sourceFile.constData(),
                             node->job->flags,
                             IndexerJob::dumpFlags(node->job->flags).constData());

        }
    }
}

void JobScheduler::abort(const std::shared_ptr<IndexerJob> &job)
{
    job->flags |= IndexerJob::Aborted;
    job->flags &= ~IndexerJob::Running;
    auto node = mActiveById.take(job->id);
    if (!node) {
        debug() << "Aborting inactive job" << job->source.sourceFile();
        node = mInactiveById.take(job->id);
    } else {
        debug() << "Aborting active job" << job->source.sourceFile();
    }
    assert(node);
    if (node->process) {
        debug() << "Killing process" << node->process;
        node->process->kill();
        mActiveByProcess.remove(node->process);
    }
}

void JobScheduler::clearHeaderError(uint32_t file)
{
    if (mHeaderErrors.remove(file))
        warning() << Location::path(file) << "was touched, starting jobs";
}
