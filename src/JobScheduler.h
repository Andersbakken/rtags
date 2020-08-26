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

#ifndef JobScheduler_h
#define JobScheduler_h

#include <assert.h>
#include <stdint.h>
#include <memory>
#include <ctime>

#include "rct/EmbeddedLinkedList.h"
#include "rct/Set.h"
#include "rct/Hash.h"
#include "rct/String.h"
#include "Source.h"
#include "IndexerJob.h"

class Connection;
class IndexDataMessage;
class IndexerJob;
class Process;
class Project;
struct DependencyNode;

class JobScheduler : public std::enable_shared_from_this<JobScheduler>
{
public:
    JobScheduler();
    ~JobScheduler();

    bool start();

    struct JobScope {
        JobScope(const std::shared_ptr<JobScheduler> &scheduler)
            : mScheduler(scheduler)
        {
            assert(mScheduler);
            ++mScheduler->mProcrastination;
        }

        ~JobScope()
        {
            if (!--mScheduler->mProcrastination)
                mScheduler->startJobs();
        }

    private:
        const std::shared_ptr<JobScheduler> mScheduler;
    };

    void add(const std::shared_ptr<IndexerJob> &job);
    void handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message);
    void dumpJobs(const std::shared_ptr<Connection> &conn);
    void dumpDaemons(const std::shared_ptr<Connection> &conn);
    void abort(const std::shared_ptr<IndexerJob> &job);
    void startJobs();
    size_t pendingJobCount() const { return mPendingJobs.size(); }
    size_t activeJobCount() const { return mActiveById.size(); }
    void sort();
private:
    bool initDaemons();
    void onProcessReadyReadStdErr(Process *process);
    void onProcessReadyReadStdOut(Process *process);
    void onProcessFinished(Process *process, pid_t pid);
    void connectProcess(Process *process);
    void jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message);
    struct Node {
        unsigned long long started { 0 };
        std::shared_ptr<IndexerJob> job;
        Process *process { nullptr };
        std::shared_ptr<Node> next, prev;
        String stdOut, stdErr;
        bool daemon { false };
    };

    int mProcrastination;
    bool mStopped;
    struct DaemonData {
        uint64_t touched { 0 };
        SourceList cache;
    };
    Hash<Process *, DaemonData> mDaemons;
    EmbeddedLinkedList<std::shared_ptr<Node> > mPendingJobs;
    Hash<Process *, std::shared_ptr<Node> > mActiveByProcess, mActiveDaemonsByProcess;
    Hash<uint64_t, std::shared_ptr<Node> > mActiveById, mInactiveById;
};

#endif
