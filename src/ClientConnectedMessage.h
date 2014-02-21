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

#ifndef ClientConnectedMessage_h
#define ClientConnectedMessage_h

#include "RTagsMessage.h"

class ClientConnectedMessage : public RTagsMessage
{
public:
    enum { MessageId = ClientConnectedId };

    ClientConnectedMessage(const String &peer = String())
        : RTagsMessage(MessageId), mPeer(peer)
    {}
    virtual void encode(Serializer &s) const
    {
        s << mPeer;
    }
    virtual void decode(Deserializer &d)
    {
        d >> mPeer;
    }

    String peer() const
    {
        return mPeer;
    }

private:
    String mPeer;
};

#endif
