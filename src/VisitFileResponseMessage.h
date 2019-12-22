/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef VisitFileResponseMessage_h
#define VisitFileResponseMessage_h

#include "RTagsMessage.h"

class VisitFileResponseMessage : public RTagsMessage
{
public:
    enum { MessageId = VisitFileResponseId };

    VisitFileResponseMessage(uint32_t fileId = 0, bool visit = false)
        : RTagsMessage(MessageId), mFileId(fileId), mVisit(visit)
    {
    }

    uint32_t fileId() const { return mFileId; }
    bool visit() const { return mVisit; }

    void encode(Serializer &serializer) const override { serializer << mFileId << mVisit; }
    void decode(Deserializer &deserializer) override { deserializer >> mFileId >> mVisit; }
private:
    uint32_t mFileId;
    bool mVisit;
};

#endif
