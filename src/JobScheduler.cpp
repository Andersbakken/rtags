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
{
}

JobScheduler::~JobScheduler()
{
    mPendingJobs.deleteAll();
    for (const auto &job : mActiveByProcess) {
        mDaemons.erase(job.first);
        job.first->kill();
        delete job.first;
    }

    for (const auto &daemon : mDaemons) {
        daemon.first->kill();
        delete daemon.first;
    }
}

void JobScheduler::setActiveJobs(size_t active)
{
    assert(active > 0);
    const bool daemon = Server::instance()->options().options & Server::RPDaemon;
    if (daemon) {
        while (mDaemons.size() > active)
            mDaemons.erase(mDaemons.begin());
    }
}

void JobScheduler::add(const std::shared_ptr<IndexerJob> &job)
{
    assert(!(job->flags & (IndexerJob::Crashed|IndexerJob::Aborted|IndexerJob::Complete|IndexerJob::Running)));
    std::shared_ptr<Node> node(new Node({ 0, job, 0, 0, 0, String(), String() }));
    node->job = job;
    // error() << job->priority << job->sourceFile << mProcrastination;
    if (mPendingJobs.isEmpty() || job->priority() > mPendingJobs.first()->job->priority()) {
        mPendingJobs.prepend(node);
    } else {
        std::shared_ptr<Node> after = mPendingJobs.last();
        while (job->priority() > after->job->priority()) {
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

void JobScheduler::startJobs()
{
    Server *server = Server::instance();
    assert(server);
    if (server->suspended()) {
        warning() << "Suspended, not starting jobs";
        return;
    }
    const auto &options = server->options();
    std::shared_ptr<Node> jobNode = mPendingJobs.first();
    auto cont = [&jobNode, this]() {
        auto tmp = jobNode->next;
        mPendingJobs.remove(jobNode);
        jobNode = tmp;
    };

    const bool isDaemon = options.options & Server::RPDaemon;
    while (mActiveByProcess.size() < options.jobCount && jobNode) {
        assert(jobNode);
        assert(jobNode->job);
        assert(!(jobNode->job->flags & (IndexerJob::Running|IndexerJob::Complete|IndexerJob::Crashed|IndexerJob::Aborted)));
        std::shared_ptr<Project> project = Server::instance()->project(jobNode->job->project);
        if (!project) {
            cont();
            debug() << jobNode->job->sourceFile << "doesn't have a project, discarding";
            continue;
        }

        switch (Server::instance()->activeBufferType(jobNode->job->sourceFileId())) {
        case Server::Active:
            jobNode->job->flags |= IndexerJob::EditorActive;
            break;
        case Server::Open:
            jobNode->job->flags |= IndexerJob::EditorOpen;
            break;
        case Server::Inactive:
            break;
        }


        const uint64_t jobId = jobNode->job->id;
        Process *process = nullptr;
        DaemonData *daemonData = nullptr;
        int daemonDataScore = -1;
        auto score = [](Flags<IndexerJob::Flag> f) -> int {
            return Flags<IndexerJob::Flag>(f & (IndexerJob::EditorActive|IndexerJob::EditorOpen)).cast();
        };

        for (auto &daemon : mDaemons) {
            if (daemon.second.cache == jobNode->job->sources) {
                process = daemon.first;
                daemonData = nullptr;
                break;
            }
            if (mDaemons.size() < options.jobCount || mActiveByProcess.find(daemon.first) != mActiveByProcess.end())
                continue; // we'll just make a new daemon, we're not at capacity

            const int dscore = score(daemon.second.flags);
            if (!daemonData || dscore < daemonDataScore || (dscore == daemonDataScore && daemon.second.touched < daemonData->touched)) {
                process = daemon.first;
                daemonData = &daemon.second;
                daemonDataScore = dscore;
            }
        }
        if (daemonData && jobNode->job->flags & (IndexerJob::EditorActive|IndexerJob::EditorOpen)) {
            daemonData->cache.clear();
            daemonData->touched = 0;
        }

        if (!process) {
            process = new Process;
            if (isDaemon)
                mDaemons[process] = DaemonData {};
            debug() << "Starting process for" << jobId << jobNode->job->sourceFileId() << jobNode->job.get();
            List<String> arguments;
            arguments << "--priority" << String::number(jobNode->job->priority());

            for (int i=logLevel().toInt(); i>0; --i)
                arguments << "-v";

            if (isDaemon)
                arguments << "--daemon";
            if (options.options & Server::RPLogToSyslog)
                arguments << "--log-to-syslog";

            process->readyReadStdErr().connect([this](Process *proc) {
                std::shared_ptr<Node> n = mActiveByProcess[proc];
                String out = proc->readAllStdErr();
                if (n)
                    n->stdErr.append(out);
            });

            process->readyReadStdOut().connect([this, isDaemon](Process *proc) {
                (void)isDaemon;
                std::shared_ptr<Node> n = mActiveByProcess.value(proc);
                if (!n) {
                    debug() << "Cannot find process in active" << proc->pid()
                            << proc->readAllStdOut();
                    startJobs();
                    return;
                }
                n->stdOut.append(proc->readAllStdOut());

                {
                    std::regex rx("@CRASH@([^@]*)@CRASH@");
                    std::smatch match;
                    while (std::regex_search(n->stdOut.ref(), match, rx)) {
                        error() << match[1].str();
                        n->stdOut.remove(match.position(), match.length());
                    }
                }
                {
                    const size_t idx = n->stdOut.indexOf("@FINISHED@");
                    if (idx != String::npos) {
                        assert(isDaemon);
                        mActiveByProcess.remove(proc);
                        assert(!n || n->process == proc);
                        if (idx > 0 || !n->stdErr.isEmpty()) {
                            error() << ("Output from " + n->job->sourceFile + ":")
                                    << '\n' << n->stdErr << n->stdOut.mid(0, idx);
                        }

                        n->stdOut.remove(0, idx + 10);
                        n->stdErr.clear();

                        assert(mDaemons.contains(n->process));
                        assert(n->process == proc);
                        n->process = 0;
                        assert(!(n->job->flags & IndexerJob::Aborted));
                        auto it = mDaemons.find(proc);
                        if (it == mDaemons.end()) {
                            proc->closeStdIn();
                        } else if (n->job->flags & (IndexerJob::EditorActive|IndexerJob::EditorOpen) || !it->second.touched) {
                            it->second.cache = n->job->sources;
                            it->second.touched = Rct::monoMs();
                            it->second.flags = n->job->flags & (IndexerJob::EditorActive|IndexerJob::EditorOpen);
                        }

                        startJobs();
                    }
                }
            });

            if (!process->start(options.rp, arguments)) {
                error() << "Couldn't start rp" << options.rp << process->errorString();
                delete process;
                if (isDaemon)
                    mDaemons.erase(process);
                jobNode->job->flags |= IndexerJob::Crashed;
                debug() << "job crashed (didn't start)" << jobId << jobNode->job->sourceFileId() << jobNode->job.get();
                auto msg = std::make_shared<IndexDataMessage>(jobNode->job);
                msg->setFlag(IndexDataMessage::ParseFailure);
                jobFinished(jobNode->job, msg);
                cont();
                continue;
            }
            const int pid = process->pid();
            process->finished().connect([this, options, pid, isDaemon](Process *proc) {
                if (isDaemon)
                    mDaemons.erase(proc);
                EventLoop::deleteLater(proc);
                auto n = mActiveByProcess.take(proc);
                assert(!n || n->process == proc);
                if (n && (!n->stdOut.isEmpty() || !n->stdErr.isEmpty())) {
                    error() << "Finish output from" << n->job->sourceFile << '\n' << n->stdErr << n->stdOut;
                }
                Path::rmdir(options.tempDir + String::number(pid));

                if (n) {
                    assert(n->process == proc);
                    n->process = 0;
                    assert(!(n->job->flags & IndexerJob::Aborted));
                    if (!(n->job->flags & IndexerJob::Complete) && proc->returnCode() != 0) {
                        auto nodeById = mActiveById.take(n->job->id);
                        assert(nodeById);
                        assert(nodeById == n);
                        // job failed, probably no IndexDataMessage coming
                        n->job->flags |= IndexerJob::Crashed;
                        debug() << "job crashed" << n->job->id << n->job->sourceFileId() << n->job.get();
                        auto msg = std::make_shared<IndexDataMessage>(n->job);
                        msg->setFlag(IndexDataMessage::ParseFailure);
                        jobFinished(n->job, msg);
                    }
                }
                startJobs();
            });
        }

        jobNode->process = process;
        assert(!(jobNode->job->flags & (IndexerJob::Crashed|IndexerJob::Aborted|IndexerJob::Complete|IndexerJob::Running)));
        jobNode->job->flags |= IndexerJob::Running;
        process->write(jobNode->job->encode());
        jobNode->started = Rct::monoMs();
        mActiveByProcess[process] = jobNode;
        mInactiveById.remove(jobId);
        mActiveById[jobId] = jobNode;
        cont();
    }
}

void JobScheduler::handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message)
{
    auto node = mActiveById.take(message->id());
    if (!node) {
        warning() << "Got IndexDataMessage for unknown job" << message->id() << mActiveById.keys();
        return;
    }
    debug() << "job got index data message" << node->job->id << node->job->sourceFileId() << node->job.get();
    jobFinished(node->job, message);
}

void JobScheduler::jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message)
{
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
        debug() << "job crashed too many times" << job->id << job->sourceFileId() << job.get();
    }
    project->onJobFinished(job, message);
}

void JobScheduler::dumpJobs(const std::shared_ptr<Connection> &conn)
{
    if (!mPendingJobs.isEmpty()) {
        conn->write("Pending:");
        for (const auto &node : mPendingJobs) {
            conn->write<128>("%s: %s %d %s",
                             node->job->sourceFile.constData(),
                             node->job->flags.toString().constData(),
                             node->job->priority(),
                             IndexerJob::dumpFlags(node->job->flags).constData());
        }
    }
    if (!mActiveById.isEmpty()) {
        conn->write("Active:");
        const unsigned long long now = Rct::monoMs();
        for (const auto &node : mActiveById) {
            conn->write<128>("%s: %s priority: %d %s %lldms",
                             node.second->job->sourceFile.constData(),
                             node.second->job->flags.toString().constData(),
                             node.second->job->priority(),
                             IndexerJob::dumpFlags(node.second->job->flags).constData(),
                             now - node.second->started);

        }
    }
}

void JobScheduler::dumpDaemons(const std::shared_ptr<Connection> &conn)
{
    if (mDaemons.size()) {
        conn->write("Daemons:");
        for (const auto &daemon : mDaemons) {
            if (!daemon.second.cache.isEmpty()) {
                conn->write<1024>("pid: %d %s%s",
                                  daemon.first->pid(),
                                  daemon.second.cache.front().sourceFile().constData(),
                                  daemon.second.cache.size() > 1 ? String::format(" (%zu builds)",
                                                                                  daemon.second.cache.size()).constData() : "");
            }
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
        debug() << "Aborting inactive job" << job->sourceFile << job->sourceFileId() << job->id << job.get();
        node = mInactiveById.take(job->id);
        assert(node);
        mPendingJobs.remove(node);
    } else {
        debug() << "Aborting active job" << job->sourceFile << job->sourceFileId() << job->id << job.get();
    }
    if (node->process) {
        if (Server::instance()->options().options & Server::RPDaemon) {
            debug() << "Killing process with SIGALRM" << node->process;
            node->process->kill(SIGALRM);
        } else {
            debug() << "Killing process" << node->process;
            node->process->kill();
        }
        mActiveByProcess.remove(node->process);
    }
}

void JobScheduler::sort()
{
    std::vector<std::shared_ptr<Node> > nodes(mPendingJobs.size());
    for (size_t i=0; i<nodes.size(); ++i) {
        std::shared_ptr<Node> node = mPendingJobs.removeFirst();
        node->job->recalculatePriority();
        nodes[i] = std::move(node);
    }

    std::stable_sort(nodes.begin(), nodes.end(), [](const std::shared_ptr<Node> &l, const std::shared_ptr<Node> &r) -> bool {
        return l->job->priority() > r->job->priority();
    });

    for (std::shared_ptr<Node> &n : nodes) {
        mPendingJobs.append(std::move(n));
    }
}
