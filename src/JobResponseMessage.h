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

class VisitFileResponseMessage : public ClientMessage
{
public:
    enum { MessageId = JobResponseId };

    VisitFileResponseMessage(const String &indexerJobData)
        : ClientMessage(MessageId), mIndexerJobData(indexerJobData)
    {
    }

    void encode(Serializer &serializer) const { serializer << mIndexerJobData; }
    void decode(Deserializer &deserializer) { deserializer >> mIndexerJobData; }
    const String &indexerJobData() const { return mIndexerJobData; }
private:
    String mIndexerJobData;
};

#endif
