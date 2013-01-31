#ifndef LogObject_h
#define LogObject_h

#include <rct/String.h>
#include <rct/Log.h>
#include "Connection.h"
#include <rct/EventReceiver.h>

class LogObject : public LogOutput, public EventReceiver
{
public:
    LogObject(Connection *conn, int level)
        : LogOutput(level), mConnection(conn)
    {
        conn->disconnected().connect(this, &LogObject::shutdown);
    }

    void shutdown()
    {
        deleteLater();
    }

    virtual void log(const char *msg, int len)
    {
        mConnection->writeAsync(String(msg, len));
    }
private:
    Connection *mConnection;
};

#endif
