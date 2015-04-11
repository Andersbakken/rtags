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

#ifndef RTagsLogOutput_h
#define RTagsLogOutput_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Connection.h>

class RTagsLogOutput : public LogOutput
{
public:
    RTagsLogOutput(int level, unsigned int flags, const std::shared_ptr<Connection> &conn = std::shared_ptr<Connection>())
        : LogOutput(level), mFlags(flags), mConnection(conn)
    {
        if (conn) {
            conn->disconnected().connect(std::bind(&RTagsLogOutput::remove, this));
        }
    }

    enum Flag {
        None = 0x0,
        ElispList = 0x1
    };

    virtual unsigned int flags() const override { return mFlags; }

    virtual bool testLog(int level) const override
    {
        if (logLevel() < 0 || level < 0)
            return level == logLevel();
        return LogOutput::testLog(level);
    }
    virtual void log(const char *msg, int len) override
    {
        if (mConnection) {
            std::shared_ptr<EventLoop> main = EventLoop::mainEventLoop();
            if (EventLoop::eventLoop() == main) {
                mConnection->write(String(msg, len));
            } else {
                EventLoop::mainEventLoop()->callLaterMove(std::bind((bool(Connection::*)(Message&&))&Connection::send, mConnection, std::placeholders::_1),
                                                          ResponseMessage(String(msg, len)));
            }
        }
    }
    std::shared_ptr<Connection> connection() const { return mConnection; }
private:
    const unsigned int mFlags;
    std::shared_ptr<Connection> mConnection;
};

#endif
