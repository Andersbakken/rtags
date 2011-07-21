#ifndef ebus_h
#define ebus_h

#include <QtCore>
#include <QtNetwork>

class Daemon;
class EBusServer : public QTcpServer
{
    Q_OBJECT;
public:
    enum { DefaultPort = 6767 };
    EBusServer(Daemon *daemon, int port = DefaultPort);
    bool start();
    void read(QTcpSocket *socket);
signals:
    void commandReceived(const QStringList &args);
public slots:
    void onNewConnection();
    void onReadyRead();
    void onDisconnected();
private:
    const int mPort;
    Daemon *mDaemon;
    QHash<QTcpSocket*, qint16> mConnections;
};

class EBusClient : public QTcpSocket
{
    Q_OBJECT;
public:
    EBusClient(const QStringList &args, int port = EBusServer::DefaultPort);
    void send();
public slots:
    void onReadyRead();
    void onConnected();
private:
    const QStringList mArguments;
    const int mPort;
    qint16 mSize;
};

#endif
