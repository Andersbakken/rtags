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

void JobResponseMessage::encode(Serializer &serializer) const
{
    serializer << preprocessed << project << source << sourceFile << blockedFiles << htons(port) << id;
}

void JobResponseMessage::decode(Deserializer &deserializer)
{
    deserializer >> preprocessed >> project >> source >> sourceFile >> blockedFiles >> port >> id;
    port = ntohs(port);
}

void JobResponseMessage::toIndexerJob(std::shared_ptr<IndexerJob>& job, Connection* conn) const
{
    job->state = IndexerJob::Pending;
    job->type = IndexerJob::Remote;
    job->preprocessed = preprocessed;
    job->project = project;
    job->source = source;
    job->sourceFile = sourceFile;
    std::string dest;
    conn->client()->peer(&dest);
    job->destination = dest;
    job->port = port;
    job->blockedFiles = blockedFiles;
    job->id = id;
}
