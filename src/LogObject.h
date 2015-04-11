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

#ifndef LogObject_h
#define LogObject_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Connection.h>

class LogObject : public LogOutput
{
public:
    LogObject(const std::shared_ptr<Connection> &conn, int level)
        : LogOutput(level), mConnection(conn)
    {
        conn->disconnected().connect(std::bind(&LogOutput::remove, this));
    }

    virtual void log(const char *msg, int len) override
    {
        EventLoop::mainEventLoop()->callLaterMove(std::bind((bool(Connection::*)(Message&&))&Connection::send, mConnection, std::placeholders::_1),
                                                  ResponseMessage(String(msg, len)));
    }

    virtual bool testLog(int level) const override
    {
        if (logLevel() < 0 || level < 0)
            return level == logLevel();
        return LogOutput::testLog(level);
    }
private:
    const std::shared_ptr<Connection> &mConnection;
};

#endif
