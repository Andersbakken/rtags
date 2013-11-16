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
#include "ClientMessage.h"
#include "IndexerJob.h"
#include "Project.h"
#include <memory>

class Connection;

class JobResponseMessage : public ClientMessage
{
public:
    enum { MessageId = JobResponseId };

    JobResponseMessage();
    JobResponseMessage(const std::shared_ptr<IndexerJob>& job, uint16_t p);
    void encode(Serializer &serializer) const;
    void decode(Deserializer &deserializer);

    void toIndexerJob(std::shared_ptr<IndexerJob>& job, Connection* conn) const;

private:
    std::shared_ptr<Cpp> cpp;
    Path project;
    Source source;
    Path sourceFile;
    uint16_t port;
    Hash<Path, uint32_t> blockedFiles;
    uint64_t id;
};

#endif
