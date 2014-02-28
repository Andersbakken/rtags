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

#ifndef IndexerMessage_h
#define IndexerMessage_h

#include <rct/Message.h>
#include <rct/String.h>
#include "RTagsMessage.h"
#include "IndexerJob.h"

class IndexerMessage : public RTagsMessage
{
public:
    enum { MessageId = IndexerMessageId };

    IndexerMessage(const Path &project, std::shared_ptr<IndexData> &data)
        : RTagsMessage(MessageId), mProject(project), mData(data)
    {
    }

    IndexerMessage()
        : RTagsMessage(MessageId)
    {}

    void encode(Serializer &serializer) const
    {
        StopWatch sw;
        assert(mData);
        serializer << mProject << mData->flags << mData->key << mData->parseTime;
        CursorInfo::serialize(serializer, mData->symbols);
        serializer << mData->references << mData->symbolNames << mData->dependencies
                   << mData->usrMap << mData->message << mData->fixIts
                   << mData->xmlDiagnostics << mData->visited << mData->jobId;
        error() << "encoding took" << sw.elapsed();
    }
    void decode(Deserializer &deserializer)
    {
        assert(!mData);
        uint32_t flags;
        deserializer >> mProject >> flags;
        mData.reset(new IndexData(flags));
        deserializer >> mData->key >> mData->parseTime;
        CursorInfo::deserialize(deserializer, mData->symbols);
        deserializer >> mData->references >> mData->symbolNames >> mData->dependencies
                     >> mData->usrMap >> mData->message >> mData->fixIts >> mData->xmlDiagnostics
                     >> mData->visited >> mData->jobId;
    }
    std::shared_ptr<IndexData> data() const { return mData; }
    const Path &project() const { return mProject; }
private:
    Path mProject;
    std::shared_ptr<IndexData> mData;
};

#endif
