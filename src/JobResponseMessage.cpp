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
    : ClientMessage(MessageId), flags(0)
{
}

JobResponseMessage::JobResponseMessage(const std::shared_ptr<IndexerJob> &job, uint16_t p)
    : ClientMessage(MessageId), port(p)
{
    cpp = job->cpp;
    project = job->project;
    source = job->source;
    sourceFile = job->sourceFile;
    id = job->id;
    flags = job->flags;
    std::shared_ptr<Project> proj = Server::instance()->project(project);
    assert(proj);
    blockedFiles = proj->visitedFiles();
}

void JobResponseMessage::encode(Serializer &serializer) const
{
    serializer << *cpp << project << source << sourceFile << blockedFiles << port << id << flags;
}

void JobResponseMessage::decode(Deserializer &deserializer)
{
    cpp.reset(new Cpp);
    deserializer >> *cpp >> project >> source >> sourceFile >> blockedFiles >> port >> id >> flags;
}

void JobResponseMessage::toIndexerJob(std::shared_ptr<IndexerJob>& job, Connection* conn) const
{
    job->flags = flags;
    job->flags &= IndexerJob::Remote;
    job->flags |= IndexerJob::FromRemote;
    job->cpp = cpp;
    job->project = project;
    job->source = source;
    job->sourceFile = sourceFile;
    String dest;
    conn->client()->peer(&dest);
    job->destination = dest;
    job->port = port;
    job->blockedFiles = blockedFiles;
    job->id = id;
}
