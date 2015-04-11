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

#include "IndexerJob.h"
#include "IndexDataMessage.h"
#include <memory>
#include <rct/EmbeddedLinkedList.h>
#include <rct/Connection.h>

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
    void clearHeaderError(uint32_t file);
    Set<uint32_t> headerErrors() const { return mHeaderErrors; }
private:
    enum { HighPriority = 5 };
    void jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &message);
    void startJobs();
    struct Node {
        std::shared_ptr<IndexerJob> job;
        Process *process;
        std::shared_ptr<Node> next, prev;
    };
    uint32_t hasHeaderError(DependencyNode *node, Set<uint32_t> &seen) const;
    uint32_t hasHeaderError(uint32_t file, const std::shared_ptr<Project> &project) const;

    int mProcrastination;
    Set<uint32_t> mHeaderErrors;
    Set<uint64_t> mHeaderErrorJobIds;
    EmbeddedLinkedList<std::shared_ptr<Node> > mPendingJobs;
    Hash<Process *, std::shared_ptr<Node> > mActiveByProcess;
    Hash<uint64_t, std::shared_ptr<Node> > mActiveById, mInactiveById;
};

#endif
