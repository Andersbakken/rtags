#ifndef CLIENT_H
#define CLIENT_H

#include <QtCore>
#ifdef EBUS_ENABLED
#include "EBus.h"
#else
class DaemonInterface;
#endif

class Client : public QObject
{
    Q_OBJECT;
public:
    Client(QObject* parent = 0);
    bool connect();
    using QObject::connect;
    bool connected() const;
    void startDaemon(const QStringList& args);

    QHash<QByteArray, QVariant> exec(const QHash<QByteArray, QVariant>& dashArgs, const QList<QByteArray>& freeArgs);
private:
#ifdef EBUS_ENABLED
    EBus ebus;
#else
    DaemonInterface* m_interface;
#endif
};

#endif
