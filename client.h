#ifndef CLIENT_H
#define CLIENT_H

#include <QtCore>
#ifdef EBUS
#include <QtNetwork>
class QTcpSocket;
#else
class DaemonInterface;
#endif

class Client : public QObject
{
    Q_OBJECT
public:
    Client(QObject* parent = 0);
    bool connect();
    using QObject::connect;
    bool connected() const;
    void startDaemon(const QStringList& args);

    QString exec(const QStringList& args);
private:
#ifdef EBUS
    QTcpSocket *m_socket;
#else
    DaemonInterface* m_interface;
#endif
};

#endif
