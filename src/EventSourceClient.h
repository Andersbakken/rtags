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

#ifndef EVENTSOURCECLIENT_H
#define EVENTSOURCECLIENT_H

#include <cstdint>
#include <rct/String.h>
#include <rct/SocketClient.h>
#include <rct/SignalSlot.h>

class EventSourceClient
{
public:
    EventSourceClient()
        : mBufferLen(0), mBuffer(0), mSeenHeaders(false)
    {}
    ~EventSourceClient();

    bool connect(const String& host, uint16_t port, const String& path);

    const SocketClient::SharedPtr& client() const { return mClient; }

    Signal<std::function<void(const String&)> >& event() { return mEvent; }

private:
    void onReadyRead(const SocketClient::SharedPtr& client, Buffer&& buffer);

private:
    SocketClient::SharedPtr mClient;
    Signal<std::function<void(const String&)> > mEvent;

    size_t mBufferLen;
    char* mBuffer;
    bool mSeenHeaders;
};

#endif
