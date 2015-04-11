/* This file is part of RTags (http://rtags.net).

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

#ifndef OutputMessage_h
#define OutputMessage_h

#include "RTagsMessage.h"
#include <rct/String.h>

class LogOutputMessage : public RTagsMessage
{
public:
    enum { MessageId = LogOutputId };

    LogOutputMessage(int level = 0, unsigned int flags = 0)
        : RTagsMessage(MessageId), mLevel(level), mFlags(flags)
    {
    }

    int level() const { return mLevel; }
    unsigned int flags() const { return mFlags; }

    void encode(Serializer &serializer) const
    {
        serializer << mRaw << mLevel << mFlags;
    }

    void decode(Deserializer &deserializer)
    {
        deserializer >> mRaw >> mLevel >> mFlags;
    }
private:
    int mLevel;
    unsigned int mFlags;
};

#endif
