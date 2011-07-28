#ifndef CLIENT_H
#define CLIENT_H

#include <QtCore>
#ifdef EBUS_ENABLED
#include <QtNetwork>
class QTcpSocket;
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

    QHash<QByteArray, QVariant> exec(const QHash<QByteArray, QVariant>& args);
private:
#ifdef EBUS_ENABLED
    QTcpSocket *m_socket;
#else
    DaemonInterface* m_interface;
#endif
};

#endif
