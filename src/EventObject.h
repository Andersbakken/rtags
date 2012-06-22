#ifndef EventObject_h
#define EventObject_h

#include <QObject>
#include <Log.h>
#include <Connection.h>

class EventObject : public QObject, public EventOutput
{
    Q_OBJECT
    Q_ENUMS(Type)
public:
    enum Type { CError = 0x40000000 };

    EventObject(Connection *conn, int level);
    virtual void log(const char *msg, int len);
    static int typeForName(const ByteArray &name);

public slots:
    void onLog(const ByteArray &log);

private:
    Connection *mConnection;
};

#endif
