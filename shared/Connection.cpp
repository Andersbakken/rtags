#include "Connection.h"
#include <QLocalSocket>

Map<int, Connection::Meta> Connection::sMetas;

class ConnectionPrivate : public QObject
{
    Q_OBJECT
public:
    ConnectionPrivate(Connection* parent)
        : QObject(parent), socket(0), conn(parent), pendingRead(0), pendingWrite(0), done(false)
    {
    }

public slots:
    void dataAvailable();
    void dataWritten(qint64 bytes);

public:
    QLocalSocket* socket;
    Connection* conn;
    quint32 pendingRead, pendingWrite;
    bool done;
};

#include "Connection.cpp.moc"

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
        ByteArray payload;
        strm >> id >> payload;
        Q_ASSERT(id > 0 && Connection::sMetas.contains(id));

        Connection::Meta m = Connection::sMetas.value(id);
        QObject* newobj = m.meta->newInstance(Q_ARG(QObject*, conn));
        m.meta->method(m.fromByteArrayId).invoke(newobj, Q_ARG(ByteArray, payload));
        emit conn->newMessage(qobject_cast<Message*>(newobj));

        pendingRead = 0;
    }
}

void ConnectionPrivate::dataWritten(qint64 bytes)
{
    Q_ASSERT(pendingWrite >= bytes);
    pendingWrite -= bytes;
    if (!pendingWrite) {
        if (bytes)
            emit conn->sendComplete();
        if (done)
            socket->close();
    }
}

Connection::Connection(QObject* parent)
    : QObject(parent), mPriv(new ConnectionPrivate(this))
{
    mPriv->socket = new QLocalSocket(mPriv);
    connect(mPriv->socket, SIGNAL(connected()), this, SIGNAL(connected()));
    connect(mPriv->socket, SIGNAL(disconnected()), this, SIGNAL(disconnected()));
    connect(mPriv->socket, SIGNAL(error(QLocalSocket::LocalSocketError)), this, SIGNAL(error()));
    connect(mPriv->socket, SIGNAL(readyRead()), mPriv, SLOT(dataAvailable()));
    connect(mPriv->socket, SIGNAL(bytesWritten(qint64)), mPriv, SLOT(dataWritten(qint64)));
}

Connection::Connection(QLocalSocket* socket, QObject* parent)
    : QObject(parent), mPriv(new ConnectionPrivate(this))
{
    Q_ASSERT(socket->state() == QLocalSocket::ConnectedState);
    socket->setParent(mPriv);
    mPriv->socket = socket;
    connect(mPriv->socket, SIGNAL(disconnected()), this, SIGNAL(disconnected()));
    connect(mPriv->socket, SIGNAL(error(QLocalSocket::LocalSocketError)), this, SIGNAL(error()));
    connect(mPriv->socket, SIGNAL(readyRead()), mPriv, SLOT(dataAvailable()));
    connect(mPriv->socket, SIGNAL(bytesWritten(qint64)), mPriv, SLOT(dataWritten(qint64)));
}


bool Connection::connectToServer(const QString &name)
{
    mPriv->socket->connectToServer(name);
    return mPriv->socket->waitForConnected(1000);
}

void Connection::send(int id, const ByteArray& message)
{
    if (mPriv->socket->state() != QLocalSocket::ConnectedState
        && mPriv->socket->state() != QLocalSocket::ConnectingState) {
        return;
    }

    QByteArray data;
    {
        QDataStream strm(&data, QIODevice::WriteOnly);
        strm << (int)0 << id << message;
        strm.device()->seek(0);
        strm << static_cast<quint32>(data.size()) - static_cast<quint32>(sizeof(quint32));
    }
    mPriv->pendingWrite += data.size();
    mPriv->socket->write(data);
}

int Connection::pendingWrite() const
{
    return mPriv->pendingWrite;
}
void Connection::finish()
{
    mPriv->done = true;
    mPriv->dataWritten(0);
}
