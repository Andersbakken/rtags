#ifndef LogObject_h
#define LogObject_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Connection.h>
#include <rct/EventReceiver.h>

class LogObject : public LogOutput, public EventReceiver
{
public:
    LogObject(Connection *conn, int level)
        : LogOutput(level), mConnection(conn)
    {
        conn->disconnected().connect(this, &LogObject::shutdown);
    }

    void shutdown(Connection *)
    {
        deleteLater();
    }

    virtual void log(const char *msg, int len)
    {
        mConnection->writeAsync(String(msg, len));
    }

    virtual bool testLog(int level) const
    {
        if (logLevel() < 0 || level < 0)
            return level == logLevel();
        return LogObject::testLog(level);
    }
private:
    Connection *mConnection;
};

#endif
