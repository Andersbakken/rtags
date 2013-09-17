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

#ifndef VisitFileMessage_h
#define VisitFileMessage_h

#include <rct/Message.h>
#include <rct/String.h>
#include "ClientMessage.h"

class VisitFileMessage : public ClientMessage
{
public:
    enum { MessageId = VisitFileId };

    VisitFileMessage(uint32_t fileId, bool visit)
        : ClientMessage(MessageId), mFileId(fileId), mVisit(visit)
    {
    }

    uint32_t fileId() const { return mFileId; }
    bool visit() const { return mVisit; }

    void encode(Serializer &serializer) const { serializer << mFileId << mVisit; }
    void decode(Deserializer &deserializer) { deserializer >> mFileId >> mVisit; }
private:
    uint32_t mFileId;
    bool mVisit;
};

#endif
