/* This file is part of RTags (http://rtags.net).

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

#include "JobScheduler.h"

#include "IndexDataMessage.h"
#include "IndexerJob.h"
#include "Project.h"
#include "rct/Connection.h"
#include "rct/Process.h"
#include "Server.h"

enum { MaxPriority = 10 };
// we set the priority to be this when a job has been requested and we couldn't load it
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
    std::shared_ptr<Node> node(new Node({ job, 0, 0, 0, String() }));
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
    Server *server = Server::instance();
    assert(server);
    if (server->suspended()) {
        warning() << "Suspended, not starting jobs";
        return;
    }
    const auto &options = server->options();
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
            headerError = hasHeaderError(node->job->fileId(), project);
            if (headerError) {
                // error() << "We got a headerError" << Location::path(headerError) << "for" << node->job->sourceFile
                //         << mHeaderErrorMaxJobs << mHeaderErrorJobIds;
                if (options.headerErrorJobCount <= mHeaderErrorJobIds.size()) {
                    warning() << "Holding off on" << node->job->sourceFile << "it's got a header error from" << Location::path(headerError);
                    node = node->next;
                    continue;
                }
            }
        }

        const uint64_t jobId = node->job->id;
        Process *process = new Process;
        debug() << "Starting process for" << jobId << node->job->fileId() << node->job.get();
        List<String> arguments;
        arguments << "--priority" << String::number(node->job->priority);

        for (int i=logLevel().toInt(); i>0; --i)
            arguments << "-v";

        process->readyReadStdOut().connect([this](Process *proc) {
                std::shared_ptr<Node> node = mActiveByProcess[proc];
                assert(node);
                node->stdOut.append(proc->readAllStdOut());

                std::regex rx("@CRASH@([^@]*)@CRASH@");
                std::smatch match;
                while (std::regex_search(node->stdOut.ref(), match, rx)) {
                    error() << match[1].str();
                    node->stdOut.remove(match.position(), match.length());
                }
            });

        if (!process->start(options.rp, arguments)) {
            error() << "Couldn't start rp" << options.rp << process->errorString();
            delete process;
            node->job->flags |= IndexerJob::Crashed;
            debug() << "job crashed (didn't start)" << jobId << node->job->fileId() << node->job.get();
            std::shared_ptr<IndexDataMessage> msg(new IndexDataMessage(node->job));
            msg->setFlag(IndexDataMessage::ParseFailure);
            jobFinished(node->job, msg);
            cont();
            continue;
        }
        if (headerError) {
            node->job->priority = IndexerJob::HeaderError;
            warning() << "Letting" << node->job->sourceFile << "go even with a headerheader error from" << Location::path(headerError);
            mHeaderErrorJobIds.insert(jobId);
        }
        process->finished().connect([this, jobId](Process *proc) {
                EventLoop::deleteLater(proc);
                auto node = mActiveByProcess.take(proc);
                assert(!node || node->process == proc);
                const String stdErr = proc->readAllStdErr();
                if ((node && !node->stdOut.isEmpty()) || !stdErr.isEmpty()) {
                    error() << (node ? ("Output from " + node->job->sourceFile + ":") : String("Orphaned process:"))
                            << '\n' << stdErr << (node ? node->stdOut : String());
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
                        debug() << "job crashed" << jobId << node->job->fileId() << node->job.get();
                        std::shared_ptr<IndexDataMessage> msg(new IndexDataMessage(node->job));
                        msg->setFlag(IndexDataMessage::ParseFailure);
                        jobFinished(node->job, msg);
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
        // error() << "STARTING JOB" << node->job->sourceFile;
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
    debug() << "job got index data message" << node->job->id << node->job->fileId() << node->job.get();
    jobFinished(node->job, message);
}

void JobScheduler::jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message)
{
    for (const auto &it : message->files()) {
        if (it.second & IndexDataMessage::HeaderError) {
            mHeaderErrors.insert(it.first);
        } else {
            mHeaderErrors.remove(it.first);
        }
    }
    // mHeaderErrors.unite(message->headerErrors());
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
        debug() << "job crashed too many times" << job->id << job->fileId() << job.get();
    }
    project->onJobFinished(job, message);
}

void JobScheduler::dump(const std::shared_ptr<Connection> &conn)
{
    if (!mPendingJobs.isEmpty()) {
        conn->write("Pending:");
        for (const auto &node : mPendingJobs) {
            conn->write<128>("%s: %s %s",
                             node->job->sourceFile.constData(),
                             node->job->flags.toString().constData(),
                             IndexerJob::dumpFlags(node->job->flags).constData());
        }
    }
    if (!mActiveById.isEmpty()) {
        conn->write("Active:");
        for (const auto &node : mActiveById) {
            conn->write<128>("%s: %s %s",
                             node.second->job->sourceFile.constData(),
                             node.second->job->flags.toString().constData(),
                             IndexerJob::dumpFlags(node.second->job->flags).constData());
        }
    }

    if (!mHeaderErrorJobIds.isEmpty()) {
        conn->write("HeaderErrorJobs:");
        for (uint64_t headerErrorJobId : mHeaderErrorJobIds) {
            auto node = mActiveById.value(headerErrorJobId);
            assert(node);
            conn->write<128>("%s: %s %s",
                             node->job->sourceFile.constData(),
                             node->job->flags.toString().constData(),
                             IndexerJob::dumpFlags(node->job->flags).constData());

        }
    }
}

void JobScheduler::abort(const std::shared_ptr<IndexerJob> &job)
{
    assert(!(job->flags & IndexerJob::Aborted));
    job->flags |= IndexerJob::Aborted;
    if (job->flags & IndexerJob::Crashed) {
        return;
    }
    job->flags &= ~IndexerJob::Running;
    auto node = mActiveById.take(job->id);
    if (!node) {
        debug() << "Aborting inactive job" << job->sourceFile << job->fileId() << job->id << job.get();
        node = mInactiveById.take(job->id);
        assert(node);
        mPendingJobs.remove(node);
    } else {
        debug() << "Aborting active job" << job->sourceFile << job->fileId() << job->id << job.get();
    }
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

// ### This is a linear lookup
bool JobScheduler::increasePriority(uint32_t fileId)
{
    for (auto node = mPendingJobs.first(); node; node = node->next) {
        if (node->job->fileId() == fileId) {
            if (node->job->priority != IndexerJob::HeaderError) {
                node->job->priority = MaxPriority;
                mPendingJobs.moveToFront(node);
                warning() << "Bumped priority for" << Location::path(fileId);
            }

            return true;
        }
    }

    for (auto pair : mActiveByProcess) {
        if (pair.second->job->fileId() == fileId) {
            warning() << Location::path(fileId) << "is already running, no need to bump priority";
            return true;
        }
    }
    debug() << Location::path(fileId) << "is not currently indexing";
    return false;
}
