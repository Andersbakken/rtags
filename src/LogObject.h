#ifndef LogObject_h
#define LogObject_h

#include <QObject>
#include <ByteArray.h>
#include <Log.h>
#include <Connection.h>

class LogObject : public QObject, public LogOutput
{
    Q_OBJECT
public:
    LogObject(Connection *conn, int level)
        : QObject(conn), LogOutput(level), mConnection(conn)
    {
        connect(conn, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
    }

    virtual void log(const char *msg, int len)
    {
        const ByteArray out(msg, len);
        QMetaObject::invokeMethod(this, "onLog", Qt::QueuedConnection, Q_ARG(ByteArray, out));
    }
public slots:
    void onLog(const ByteArray &log)
    {
        ResponseMessage msg(log);
        mConnection->send(&msg);
    }
private:
    Connection *mConnection;
};

#endif
