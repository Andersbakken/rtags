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

#ifndef JobScheduler_h
#define JobScheduler_h

#include "IndexerJob.h"
#include "IndexerMessage.h"
#include <memory>
#include <rct/EmbeddedLinkedList.h>
#include <rct/Connection.h>

class JobScheduler
{
public:
    JobScheduler(int maxJobs);
    ~JobScheduler();
    int maxJobs() const { return mMaxJobs; }
    void setMaxJobs(int maxJobs) { mMaxJobs = maxJobs; }

    void add(const std::shared_ptr<IndexerJob> &job);
    void handleIndexerMessage(const std::shared_ptr<IndexerMessage> &message);
    void dump(Connection *conn);
    void abort(const std::shared_ptr<IndexerJob> &job);
private:
    void jobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexData> &data);

    static std::shared_ptr<IndexData> createData(const std::shared_ptr<IndexerJob> &job);
    void startJobs();
    struct Node {
        std::shared_ptr<IndexerJob> job;
        Process *process;
        std::shared_ptr<Node> next, prev;
    };

    int mMaxJobs;
    EmbeddedLinkedList<std::shared_ptr<Node> > mPendingJobs;
    Hash<Process *, std::shared_ptr<Node> > mActiveByProcess;
    Hash<uint64_t, std::shared_ptr<Node> > mActiveById;
};

#endif
