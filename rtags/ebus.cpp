#include "ebus.h"
#include "daemon.h"

template <typename T>
static bool write(QTcpSocket *sock, const T &t)
{
    QByteArray byteArray;
    QDataStream ds(&byteArray, QIODevice::WriteOnly);
    ds << qint16(0);
    qint16 size = byteArray.size();
    ds << t;
    size = byteArray.size() - size; // size of QByteArray only
    ds.device()->seek(0);
    ds << qint16(size);
    return (sock->write(byteArray) == byteArray.size());
}

EBusServer::EBusServer(Daemon *daemon, int port)
    : mPort(port), mDaemon(daemon)
{
    connect(this, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
}

bool EBusServer::start()
{
    return listen(QHostAddress::LocalHost, mPort);
}

void EBusServer::onNewConnection()
{
    Q_ASSERT(hasPendingConnections());
    QTcpSocket *sock = nextPendingConnection();
    Q_ASSERT(sock);
    mConnections[sock] = -1;
    connect(sock, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    connect(sock, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    read(sock);
}
void EBusServer::onReadyRead()
{
    read(qobject_cast<QTcpSocket*>(sender()));
}

void EBusServer::onDisconnected()
{
    QTcpSocket *sock = qobject_cast<QTcpSocket*>(sender());
    disconnect(sock, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    disconnect(sock, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    mConnections.remove(sock);
    sock->deleteLater();
}
void EBusServer::read(QTcpSocket *socket)
{
    Q_ASSERT(socket);
    Q_ASSERT(mConnections.contains(socket));
    qint16 &size = mConnections[socket];
    const int available = socket->bytesAvailable();
    if (size == -2) // error
        return;
    if (size == -1) {
        if (available < qint16(sizeof(size))) {
            return;
        }
        const qint64 r = socket->read(reinterpret_cast<char*>(&size), sizeof(size));
        if (r != sizeof(size)) {
            qWarning("Read error, Expected %d, got %lld. Disconnecting", int(sizeof(size)), r);
            size = -2;
            socket->disconnect();
            return;
        }
        read(socket);
    }
    Q_ASSERT(size >= 0);
    if (available >= size) { // Could it be > size?
        QDataStream ds(socket);
        QStringList args;
        ds >> args;
        const QString ret = mDaemon->runCommand(args);
        if (!write(socket, ret)) {
            qWarning("Failed to write to socket");
        }
        disconnect(socket, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
        disconnect(socket, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
        mConnections.remove(socket);
    }
}
EBusClient::EBusClient(const QStringList &args, int port)
    : mArguments(args), mPort(port), mSize(-1)
{
    connect(this, SIGNAL(connected()), this, SLOT(onConnected()));
    connect(this, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
}

void EBusClient::send()
{
    connectToHost(QHostAddress::LocalHost, mPort);
}

void EBusClient::onConnected()
{
    if (!::write(this, mArguments)) {
        qWarning("Failed to write to socket");
        disconnect();
    }
}

void EBusClient::onReadyRead()
{
    const int available = bytesAvailable();
    if (mSize == -2)
        return;
    if (mSize == -1) {
        const qint64 r = read(reinterpret_cast<char*>(&mSize), sizeof(mSize));
        if (r != sizeof(mSize)) {
            qWarning("Read error, Expected %d, got %lld. Disconnecting", int(sizeof(mSize)), r);
            mSize = -2;
            disconnect();
            return;
        }
        onReadyRead();
        return;
    }
    if (available >= mSize) {
        QDataStream ds(this);
        QString returnValue;
        ds >> returnValue;
        disconnect();
    }

}


