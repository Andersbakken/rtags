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

#ifndef JobScheduler_h
#define JobScheduler_h

#include <memory>

#include "rct/EmbeddedLinkedList.h"
#include "rct/Set.h"
#include "rct/Hash.h"
#include "rct/String.h"
#include "rct/Path.h"
#include "rct/SignalSlot.h"

class Connection;
class IndexDataMessage;
class IndexerJob;
class Project;
struct DependencyNode;

class Vehicle : public std::enable_shared_from_this<Vehicle>
{
public:
    virtual ~Vehicle() {}

    Signal<std::function<void(const std::shared_ptr<Vehicle> &)> > &readyReadStdOut() { return mReadyReadStdOut; }
    Signal<std::function<void(const std::shared_ptr<Vehicle> &)> > &finished() { return mFinished; }

    virtual void kill() = 0;
    virtual String readAllStdOut() = 0;
    virtual String readAllStdErr() = 0;
    virtual String errorString() const = 0;
    virtual unsigned long long id() const = 0;
    virtual int returnCode() const = 0;
    virtual bool start(const std::shared_ptr<IndexerJob> &job) = 0;
private:
    Signal<std::function<void(const std::shared_ptr<Vehicle> &)> > mReadyReadStdOut, mFinished;
};
class JobScheduler : public std::enable_shared_from_this<JobScheduler>
{
public:
    JobScheduler();
    ~JobScheduler();

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
    void dump(const std::shared_ptr<Connection> &conn);
    void abort(const std::shared_ptr<IndexerJob> &job);
    void startJobs();
    size_t pendingJobCount() const { return mPendingJobs.size(); }
    size_t activeJobCount() const { return mActiveById.size(); }
    void sort();
private:
    void jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message);
    struct Node {
        unsigned long long started;
        std::shared_ptr<IndexerJob> job;
        std::shared_ptr<Vehicle> vehicle;
        std::shared_ptr<Node> next, prev;
        String stdOut;
    };

    int mProcrastination;
    EmbeddedLinkedList<std::shared_ptr<Node> > mPendingJobs;
    Hash<std::shared_ptr<Vehicle>, std::shared_ptr<Node> > mActiveByVehicle;
    Hash<uint64_t, std::shared_ptr<Node> > mActiveById, mInactiveById;
};

#endif
