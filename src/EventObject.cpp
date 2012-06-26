#include "EventObject.h"

EventObject::EventObject(Connection *conn, int level)
    : EventOutput(level), mConnection(conn)
{
    conn->disconnected().connect(static_cast<QObject*>(this), &QObject::deleteLater);
}

void EventObject::log(const char *msg, int len)
{
    const ByteArray out(msg, len);
    QMetaObject::invokeMethod(this, "onLog", Qt::QueuedConnection, Q_ARG(ByteArray, out));
}

void EventObject::onLog(const ByteArray &log)
{
    ResponseMessage msg(log);
    mConnection->send(&msg);
}

int EventObject::typeForName(const ByteArray &name)
{
    const QMetaObject m = staticMetaObject;
    for (int i = 0; i < m.enumeratorCount(); ++i) {
        const int idx = m.indexOfEnumerator("Type");
        if (idx >= 0) {
            const QMetaEnum en = m.enumerator(idx);
            if (name.contains('|'))
                return en.keysToValue(name.constData());
            else
                return en.keyToValue(name.constData());
        }
    }
    return -1;
}
