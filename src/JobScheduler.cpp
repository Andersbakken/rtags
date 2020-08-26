/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "JobScheduler.h"

#include <signal.h>
#include <algorithm>
#include <cstdint>
#include <functional>
#include <regex>
#include <unordered_map>
#include <utility>
#include <vector>

#include "IndexDataMessage.h"
#include "IndexerJob.h"
#include "Project.h"
#include "rct/Connection.h"
#include "rct/Process.h"
#include "Server.h"
#include "rct/EventLoop.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Rct.h"
#include "rct/SignalSlot.h"
#include "rct/Timer.h"

enum { MaxPriority = 10 };
// we set the priority to be this when a job has been requested and we couldn't load it
JobScheduler::JobScheduler()
    : mProcrastination(0), mStopped(false)
{
}

JobScheduler::~JobScheduler()
{
    mStopped = true;
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

bool JobScheduler::start()
{
    return initDaemons();
}

bool JobScheduler::initDaemons()
{
    const auto &options = Server::instance()->options();
    assert(mDaemons.size() <= static_cast<size_t>(options.daemonCount));
    const int needed = options.daemonCount - static_cast<int>(mDaemons.size());
    for (int i=0; i<needed; ++i) {
        Process *process = new Process;
        connectProcess(process);
        debug() << "Starting daemon" << i << process;
        List<String> arguments;
        for (int l=logLevel().toInt(); l>0; --l)
            arguments << "-v";
        arguments << "--daemon";
        if (options.options & Server::RPLogToSyslog)
            arguments << "--log-to-syslog";
        if (!process->start(options.rp, arguments)) {
            error() << "Couldn't start rp" << options.rp << process->errorString();
            delete process;
            return false;
        }
        mDaemons[process] = {};
    }
    return true;
}

void JobScheduler::add(const std::shared_ptr<IndexerJob> &job)
{
    assert(!(job->flags & (IndexerJob::Crashed|IndexerJob::Aborted|IndexerJob::Complete|IndexerJob::Running)));
    std::shared_ptr<Node> node(new Node);
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
    int slots = std::max<int>(0, options.jobCount - mActiveByProcess.size());
    int daemonSlots = std::max<int>(0, options.daemonCount - mActiveDaemonsByProcess.size());

    debug() << "JobScheduler::startJobs" << "jobCount" << options.jobCount << "active" << mActiveByProcess.size() << "\n"
            << "slots" << slots << "daemonCount" << options.daemonCount << "active daemons" << mActiveDaemonsByProcess.size() << "\n"
            << "daemonSlots" << daemonSlots;

    if (options.jobCount < mActiveByProcess.size()) {
        List<std::shared_ptr<Node> > nodes;
        nodes.reserve(mActiveByProcess.size());
        for (const auto &pair : mActiveByProcess) {
            nodes.push_back(pair.second);
        }
        std::sort(nodes.begin(), nodes.end(), [](const std::shared_ptr<Node> &l, const std::shared_ptr<Node> &r) -> bool {
            return l->started > r->started;
        });
        const size_t c = mActiveByProcess.size() - options.jobCount;
        for (size_t i=0; i<c; ++i) {
            debug() << "Killing process" << nodes[i]->started;
            nodes[i]->process->kill();
        }
    }
    std::shared_ptr<Node> node = mPendingJobs.first();
    while (node && (slots || daemonSlots)) {
        const Server::ActiveBufferType type = Server::instance()->activeBufferType(node->job->sourceFileId());
        if (daemonSlots && type == Server::Active) {
            auto cand = mDaemons.end();
            bool cacheHit = false;
            for (auto it = mDaemons.begin(); it != mDaemons.end(); ++it) {
                if (mActiveDaemonsByProcess.contains(it->first))
                    continue;

                if (node->job->sources == it->second.cache) {
                    cand = it;
                    cacheHit = true;
                    break;
                } else if (cand == mDaemons.end() || cand->second.touched > it->second.touched) {
                    cand = it;
                }
            }
            if (cand != mDaemons.end()) {
                if (!cacheHit) {
                    cand->second.touched = 0;
                    cand->second.cache.clear();
                }
                node->process = cand->first;
                assert(!(node->job->flags & (IndexerJob::Crashed|IndexerJob::Aborted|IndexerJob::Complete|IndexerJob::Running)));
                node->job->flags |= IndexerJob::Running|IndexerJob::EditorActive;
                node->daemon = true;
                debug() << "starting daemon job" << node->job->sourceFile;
                cand->first->write(node->job->encode());
                node->started = Rct::monoMs();
                mActiveDaemonsByProcess[cand->first] = node;
                mInactiveById.remove(node->job->id);
                mActiveById[node->job->id] = node;
                --daemonSlots;
                std::shared_ptr<Node> tmp = node;
                node = node->next;
                mPendingJobs.remove(tmp);
                continue;
            }
        }
        if (slots) {
            Process *process = new Process;
            debug() << "Starting process for" << node->job->id << node->job->sourceFile << node->job.get();
            List<String> arguments;
            arguments << "--priority" << String::number(node->job->priority());
            for (int i=logLevel().toInt(); i>0; --i)
                arguments << "-v";
            if (options.options & Server::RPLogToSyslog)
                arguments << "--log-to-syslog";

            connectProcess(process);

            switch (type) {
            case Server::Active:
                node->job->flags |= IndexerJob::EditorActive;
                break;
            case Server::Open:
                node->job->flags |= IndexerJob::EditorOpen;
                break;
            case Server::Inactive:
                break;
            }

            if (!process->start(options.rp, arguments)) {
                error() << "Couldn't start rp" << options.rp << process->errorString();
                delete process;
                node->job->flags |= IndexerJob::Crashed;
                debug() << "job crashed (didn't start)" << node->job->id << node->job->sourceFileId() << node->job.get();
                auto msg = std::make_shared<IndexDataMessage>(node->job);
                msg->setFlag(IndexDataMessage::ParseFailure);
                jobFinished(node->job, msg);
            } else {
                node->process = process;
                assert(!(node->job->flags & (IndexerJob::Crashed|IndexerJob::Aborted|IndexerJob::Complete|IndexerJob::Running)));
                node->job->flags |= IndexerJob::Running;
                process->write(node->job->encode());
                node->started = Rct::monoMs();
                mActiveByProcess[process] = node;
                mActiveById[node->job->id] = node;
                --slots;
            }
            mInactiveById.remove(node->job->id);
            std::shared_ptr<Node> tmp = node;
            node = node->next;
            mPendingJobs.remove(tmp);
        } else {
            node = node->next;
        }
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
    conn->write<1024>("Pending: %zu", mPendingJobs.size());
    if (!mPendingJobs.isEmpty()) {
        for (const auto &node : mPendingJobs) {
            conn->write<128>("%s: %s %d %s",
                             node->job->sourceFile.constData(),
                             node->job->flags.toString().constData(),
                             node->job->priority(),
                             IndexerJob::dumpFlags(node->job->flags).constData());
        }
    }

    conn->write<1024>("Active: %zu/%zu", mActiveById.size(), Server::instance()->options().jobCount);
    if (!mActiveById.isEmpty()) {
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
        conn->write<1024>("Daemons: %zu", mDaemons.size());
        for (const auto &daemon : mDaemons) {
            if (!daemon.second.cache.isEmpty()) {
                conn->write<1024>("pid: %d %s%s",
                                  static_cast<int>(daemon.first->pid()),
                                  daemon.second.cache.front().sourceFile().constData(),
                                  daemon.second.cache.size() > 1 ? String::format(" (%zu builds)",
                                                                                  daemon.second.cache.size()).constData() : "");
            } else {
                conn->write<1024>("pid: %d: empty", static_cast<int>(daemon.first->pid()));
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
        if (node->daemon) {
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

void JobScheduler::onProcessReadyReadStdErr(Process *proc)
{
    std::shared_ptr<Node> n = mActiveByProcess.value(proc);
    String out = proc->readAllStdErr();
    if (n)
        n->stdErr.append(out);
}

void JobScheduler::onProcessReadyReadStdOut(Process *proc)
{
    std::shared_ptr<Node> n = mActiveByProcess.value(proc);
    bool daemon = false;
    if (!n) {
        n = mActiveDaemonsByProcess.value(proc);
        daemon = true;
    }
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
    if (daemon) {
        const size_t idx = n->stdOut.indexOf("@FINISHED@");
        if (idx != String::npos) {
            const bool removed = mActiveDaemonsByProcess.remove(proc);
            static_cast<void>(removed);
            assert(removed);
            assert(n->process == proc);
            if (idx > 0 || !n->stdErr.isEmpty()) {
                error() << ("Output from " + n->job->sourceFile + ":")
                        << '\n' << n->stdErr << n->stdOut.mid(0, idx);
            }

            n->stdOut.remove(0, idx + 10);
            n->stdErr.clear();
            assert(mDaemons.contains(n->process));

            DaemonData &data = mDaemons[n->process];
            data.cache = n->job->sources;
            data.touched = Rct::monoMs();
            assert(n->process == proc);
            n->process = nullptr;
            assert(!(n->job->flags & IndexerJob::Aborted));
            startJobs();
        }
    }
}

void JobScheduler::onProcessFinished(Process *proc, pid_t pid)
{
    const bool daemon = mDaemons.erase(proc);
    // ### restart daemon?
    EventLoop::deleteLater(proc);
    auto n = mActiveByProcess.take(proc);
    if (!n) {
        n = mActiveDaemonsByProcess.take(proc);
    }
    if (n && (!n->stdOut.isEmpty() || !n->stdErr.isEmpty())) {
        error() << "Finish output from" << n->job->sourceFile << '\n' << n->stdErr << n->stdOut;
    }
    const auto &options = Server::instance()->options();
    Path::rmdir(options.tempDir + String::number(pid));

    if (n) {
        assert(n->process == proc);
        n->process = nullptr;
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
    if (!mStopped) {
        if (daemon)
            initDaemons();
        startJobs();
    }
}

void JobScheduler::connectProcess(Process *process)
{
    assert(process);
    process->readyReadStdOut().connect(std::bind(&JobScheduler::onProcessReadyReadStdOut, this, std::placeholders::_1));
    process->readyReadStdErr().connect(std::bind(&JobScheduler::onProcessReadyReadStdErr, this, std::placeholders::_1));
    process->finished().connect(std::bind(&JobScheduler::onProcessFinished, this, std::placeholders::_1, std::placeholders::_2));
}
