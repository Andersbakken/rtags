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

#ifndef ExitCommand_h
#define ExitCommand_h

#include "RTagsMessage.h"

class ExitMessage : public RTagsMessage
{
public:
    enum { MessageId = ExitMessageId };

    ExitMessage(int exitCode = 0, bool forward = false)
        : RTagsMessage(MessageId), mExitCode(exitCode), mForward(forward)
    {
    }

    int exitCode() const { return mExitCode; }
    bool forward() const { return mForward; }

    virtual void encode(Serializer &serializer) const
    {
        serializer << mExitCode << mForward;
    }
    virtual void decode(Deserializer &deserializer)
    {
        deserializer >> mExitCode >> mForward;
    }
private:
    int mExitCode;
    bool mForward;
};

#endif

