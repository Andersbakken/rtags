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

#include "JobResponseMessage.h"
#include <rct/Serializer.h>
#include <rct/Connection.h>
#include <arpa/inet.h>
#include "Cpp.h"
#include "Server.h"

JobResponseMessage::JobResponseMessage()
    : RTagsMessage(MessageId), mPort(0), mFinished(false)
{
}

JobResponseMessage::JobResponseMessage(const List<std::shared_ptr<IndexerJob> > &jobs, uint16_t p, bool finished)
    : RTagsMessage(MessageId), mPort(p), mFinished(finished)
{
    const int count = jobs.size();
    mJobData.resize(count);

    for (int i=0; i<count; ++i) {
        const std::shared_ptr<IndexerJob> &job = jobs[i];
        auto &jobData = mJobData[i];
        jobData.cpp = job->cpp;
        jobData.project = job->project;
        jobData.source = job->source;
        jobData.sourceFile = job->sourceFile;
        jobData.id = job->id;
        jobData.flags = job->flags;
        if (auto proj = Server::instance()->project(job->project)) {
            // not sure if the else case should be possible
            jobData.blockedFiles = proj->visitedFiles();
        }
    }
}

void JobResponseMessage::encode(Serializer &serializer) const
{
    serializer << mPort << mFinished << static_cast<uint32_t>(mJobData.size());
    for (const auto &job : mJobData) {
        serializer << *job.cpp << job.project << job.source << job.sourceFile
                   << job.id << job.flags << job.blockedFiles;
    }
}

void JobResponseMessage::decode(Deserializer &deserializer)
{
    assert(mJobData.isEmpty());
    uint32_t count;
    deserializer >> mPort >> mFinished >> count;
    mJobData.resize(count);

    for (uint32_t i=0; i<count; ++i) {
        JobData &jobData = mJobData[i];
        jobData.cpp.reset(new Cpp);
        deserializer >> *jobData.cpp >> jobData.project >> jobData.source >> jobData.sourceFile
                     >> jobData.id >> jobData.flags >> jobData.blockedFiles;
    }
}

List<std::shared_ptr<IndexerJob> > JobResponseMessage::jobs(const String &host) const
{
    List<std::shared_ptr<IndexerJob> > ret(mJobData.size());
    assert(!ret.isEmpty());
    const int count = ret.size();
    for (int i=0; i<count; ++i) {
        auto &job = ret[i];
        const auto &jobData = mJobData[i];
        job.reset(new IndexerJob);
        job->flags = jobData.flags;
        job->flags &= ~IndexerJob::Remote;
        job->flags |= IndexerJob::FromRemote;
        job->cpp = jobData.cpp;
        job->project = jobData.project;
        job->source = jobData.source;
        job->sourceFile = jobData.sourceFile;
        job->destination = host;
        job->port = mPort;
        job->blockedFiles = jobData.blockedFiles;
        job->id = jobData.id;
    }
    return ret;
}
