#ifndef LogObject_h
#define LogObject_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Connection.h>

class LogObject : public LogOutput
{
public:
    LogObject(Connection *conn, int level)
        : LogOutput(level), mConnection(conn)
    {
        conn->disconnected().connect(std::bind(&LogObject::shutdown, this));
    }

    void shutdown()
    {
        EventLoop::deleteLater(this);
    }

    virtual void log(const char *msg, int len)
    {
        mConnection->writeAsync(String(msg, len));
    }

    virtual bool testLog(int level) const
    {
        if (logLevel() < 0 || level < 0)
            return level == logLevel();
        return LogOutput::testLog(level);
    }
private:
    Connection *mConnection;
};

#endif
