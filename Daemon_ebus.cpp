#ifndef EBUS_ENABLED
#error This file should only be included if EBUS is defined
#endif
#include "Utils.h"
#include "Daemon.h"
#include "Daemon_p.h"

using namespace EBus;
void Daemon::onNewConnection()
{
    FUNC;
    Q_ASSERT(m_server->hasPendingConnections());
    QTcpSocket *sock = m_server->nextPendingConnection();
    Q_ASSERT(sock);
    m_connections[sock] = -1;
    connect(sock, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    connect(sock, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    read(sock);
}

void Daemon::onReadyRead()
{
    FUNC;
    read(qobject_cast<QTcpSocket*>(sender()));
}

void Daemon::onDisconnected()
{
    FUNC;
    QTcpSocket *sock = qobject_cast<QTcpSocket*>(sender());
    disconnect(sock, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    disconnect(sock, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    m_connections.remove(sock);
    sock->deleteLater();
}

void Daemon::read(QTcpSocket *socket)
{
    FUNC1(socket);
    Q_ASSERT(socket);
    Q_ASSERT(m_connections.contains(socket));
    QStringList arguments;
    qint16 &size = m_connections[socket];
    switch (readFromSocket(socket, arguments, size)) {
    case Error:
        qWarning("Couldn't send message to daemon");
        socket->disconnect();
        break;
    case WaitForData:
        break;
    case Finished:
        ::writeToSocket(socket, runCommand(arguments));
        break;
    }
}
