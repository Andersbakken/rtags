#ifndef LogObject_h
#define LogObject_h

#include "ByteArray.h"
#include "Log.h"
#include "Connection.h"
#include "EventReceiver.h"

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
        mConnection->writeAsync(ByteArray(msg, len));
    }
private:
    Connection *mConnection;
};

#endif
