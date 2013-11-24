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

#ifndef MulticastForwardMessage_h
#define MulticastForwardMessage_h

#include <rct/Message.h>
#include <rct/String.h>
#include "ClientMessage.h"

class MulticastForwardMessage : public ClientMessage
{
public:
    enum { MessageId = MulticastForwardId };

    MulticastForwardMessage(const std::string &ip = String(), uint16_t port = 0, const String &message = String())
        : ClientMessage(MessageId), mIp(ip), mPort(port), mMessage(message)
    {}

    void encode(Serializer &serializer) const
    {
        serializer << mIp << mPort << mMessage;
    }

    void decode(Deserializer &deserializer)
    {
        deserializer >> mIp >> mPort >> mMessage;
    }
    const String &ip() const { return mIp; }
    uint16_t port() const { return mPort; }
    const String &message() const { return mMessage; }
private:
    String mIp;
    uint16_t mPort;
    String mMessage;
};

#endif
