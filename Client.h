#ifndef CLIENT_H
#define CLIENT_H

#include <QtCore>
#include "EBus.h"
class Client : public QObject
{
    Q_OBJECT;
public:
    Client(QObject* parent = 0);
    bool connect(int timeout);
    using QObject::connect;
    bool isConnected() const;
    void startDaemon(const QStringList& args);

    QHash<QByteArray, QVariant> exec(const QHash<QByteArray, QVariant>& dashArgs, const QList<QByteArray>& freeArgs);
private:
    EBus mEbus;
};

#endif
