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

#ifndef JobResponseMessage_h
#define JobResponseMessage_h

#include <rct/Message.h>
#include <rct/String.h>
#include "RTagsMessage.h"
#include "IndexerJob.h"
#include "Project.h"
#include <memory>

class Connection;

class JobResponseMessage : public RTagsMessage
{
public:
    enum { MessageId = JobResponseId };

    JobResponseMessage();
    JobResponseMessage(const List<std::shared_ptr<IndexerJob> >& job, uint16_t p, bool finished);

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);

    bool isFinished() const { return mFinished; }
    List<std::shared_ptr<IndexerJob> > jobs(const String &host) const;
private:
    struct JobData {
        std::shared_ptr<Cpp> cpp;
        Path project;
        Source source;
        Path sourceFile;
        Hash<uint32_t, Path> blockedFiles;
        uint64_t id;
        uint32_t flags;
    };

    uint16_t mPort;
    bool mFinished;
    List<JobData> mJobData;
};

#endif
