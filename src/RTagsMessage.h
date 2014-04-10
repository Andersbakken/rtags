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

#ifndef RTagsMessage_h
#define RTagsMessage_h

#include <rct/Message.h>
#include <rct/String.h>

class RTagsMessage : public Message
{
public:
    enum {
        CompletionId = 32,
        QueryId,
        CompileId,
        LogOutputId,
        VisitFileResponseId,
        VisitFileId,
        IndexerMessageId,
        JobRequestId,
        JobResponseId,
        JobAnnouncementId,
        ProxyJobAnnouncementId,
        ClientMessageId,
        ClientConnectedId,
        ExitMessageId
    };

    RTagsMessage(uint8_t id) : Message(id) {}

    inline void init(int argc, const char **argv)
    {
        mRaw.reserve(256);
        for (int i=0; i<argc; ++i) {
            if (i > 0)
                mRaw.append(' ');
            const bool space = strchr(argv[i], ' ');
            if (space)
                mRaw.append('"');
            mRaw.append(argv[i]);
            if (space)
                mRaw.append('"');
        }
    }
    inline void init(int argc, char **argv)
    {
        init(argc, const_cast<const char**>(argv));
    }

    inline String raw() const
    {
        return mRaw;
    }
protected:
    String mRaw;
};

#endif
