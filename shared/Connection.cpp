#include "Connection.h"
#include <QTcpSocket>

QHash<int, Connection::Meta> Connection::s_metas;

class ConnectionPrivate : public QObject
{
    Q_OBJECT
public:
    ConnectionPrivate(Connection* parent)
        : QObject(parent), socket(0), conn(parent), pendingRead(0), pendingWrite(0)
    {
    }

public slots:
    void dataAvailable();
    void dataWritten(qint64 bytes);

public:
    QTcpSocket* socket;
    Connection* conn;
    quint32 pendingRead, pendingWrite;
};

void ConnectionPrivate::dataAvailable()
{
    for (;;) {
        if (!pendingRead &&
            socket->bytesAvailable() < static_cast<int>(sizeof(quint32)))
            return;
        QDataStream strm(socket);
        if (!pendingRead)
            strm >> pendingRead;
        if (socket->bytesAvailable() < pendingRead)
            return;

        int id;
        QByteArray payload;
        strm >> id >> payload;
        Q_ASSERT(id > 0 && Connection::s_metas.contains(id));

        Connection::Meta m = Connection::s_metas.value(id);
        QObject* newobj = m.meta->newInstance(Q_ARG(QObject*, conn));
        m.meta->method(m.fromByteArrayId).invoke(newobj, Q_ARG(QByteArray, payload));
        emit conn->newMessage(qobject_cast<Message*>(newobj));

        pendingRead = 0;
    }
}

void ConnectionPrivate::dataWritten(qint64 bytes)
{
    Q_ASSERT(pendingWrite >= bytes);
    pendingWrite -= bytes;
    if (!pendingWrite)
        emit conn->sendComplete();
}

#include "Connection.moc"

Connection::Connection(QObject* parent)
    : QObject(parent), m_priv(new ConnectionPrivate(this))
{
    m_priv->socket = new QTcpSocket(m_priv);
    connect(m_priv->socket, SIGNAL(connected()), this, SIGNAL(connected()));
    connect(m_priv->socket, SIGNAL(disconnected()), this, SIGNAL(disconnected()));
    connect(m_priv->socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SIGNAL(error()));
    connect(m_priv->socket, SIGNAL(readyRead()), m_priv, SLOT(dataAvailable()));
    connect(m_priv->socket, SIGNAL(bytesWritten(qint64)), m_priv, SLOT(dataWritten(qint64)));
}

Connection::Connection(QTcpSocket* socket, QObject* parent)
    : QObject(parent), m_priv(new ConnectionPrivate(this))
{
    Q_ASSERT(socket->state() == QAbstractSocket::ConnectedState);
    socket->setParent(m_priv);
    m_priv->socket = socket;
    connect(m_priv->socket, SIGNAL(disconnected()), this, SIGNAL(disconnected()));
    connect(m_priv->socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SIGNAL(error()));
    connect(m_priv->socket, SIGNAL(readyRead()), m_priv, SLOT(dataAvailable()));
    connect(m_priv->socket, SIGNAL(bytesWritten(qint64)), m_priv, SLOT(dataWritten(qint64)));
}

void Connection::connectToHost(const QString& host, quint16 port)
{
    m_priv->socket->connectToHost(host, port);
}

void Connection::send(int id, const QByteArray& message)
{
    if (m_priv->socket->state() != QAbstractSocket::ConnectedState
        && m_priv->socket->state() != QAbstractSocket::ConnectingState
        && m_priv->socket->state() != QAbstractSocket::HostLookupState)
        return;

    QByteArray data;
    {
        QDataStream strm(&data, QIODevice::WriteOnly);
        strm << (int)0 << id << message;
        strm.device()->seek(0);
        strm << static_cast<quint32>(data.size()) - static_cast<quint32>(sizeof(quint32));
    }
    m_priv->pendingWrite += data.size();
    m_priv->socket->write(data);
}

int Connection::pendingWrite() const
{
    return m_priv->pendingWrite;
}
