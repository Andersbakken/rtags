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

#ifndef RTagsMessage_h
#define RTagsMessage_h

#include "rct/Message.h"
#include "rct/String.h"
#include <cstdint>
#include <string.h>

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
        IndexDataMessageId,
        JobRequestId,
        JobResponseId,
        JobAnnouncementId,
        ProxyJobAnnouncementId,
        ClientMessageId,
        ClientConnectedId,
        ExitMessageId
    };

    RTagsMessage(uint8_t id) : Message(id) {}

    inline void setCommandLine(const String &cmd) { mCommandLine = cmd; }
    inline String commandLine() const { return mCommandLine; }
protected:
    String mCommandLine;
};

#endif
