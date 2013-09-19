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

#ifndef IndexerJobClang_h
#define IndexerJobClang_h

#include "IndexerJob.h"
#include <ProcessPool.h>

class IndexerMessage;
class IndexerJobClang : public IndexerJob, public ProcessPool::Job
{
public:
    IndexerJobClang(uint64_t id, IndexType type, const std::shared_ptr<Project> &project,
                    const SourceInformation &sourceInformation);
    IndexerJobClang(const QueryMessage &msg,
                    const std::shared_ptr<Project> &project,
                    const SourceInformation &sourceInformation);
    virtual void start();
    virtual bool abort();

    virtual bool isAborted() const { return mState == Aborted; }

    // ProcessPool::Job
    virtual bool init(Path &path, List<String> &args, String &data);
    virtual void error(const String &error);
    virtual void finished(Process* process);
private:
    enum State {
        Pending,
        Running,
        Aborted
    } mState;
    bool mStarted;
};

#endif
