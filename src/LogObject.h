#ifndef LogObject_h
#define LogObject_h

#include <ByteArray.h>
#include <Log.h>
#include <Connection.h>

class LogObject : public LogOutput
{
public:
    LogObject(Connection *conn, int level)
        : LogOutput(level), mConnection(conn)
    {
        conn->disconnected().connect(this, &LogObject::shutdown);
    }

    void shutdown()
    {
        delete this;
    }

    virtual void log(const char *msg, int len)
    {
        mConnection->write(ByteArray(msg, len));
    }
private:
    Connection *mConnection;
};

#endif
