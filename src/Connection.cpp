#include "Connection.h"
#include <LocalClient.h>

Map<int, Connection::Meta> Connection::sMetas;

class ConnectionPrivate : public QObject
{
    Q_OBJECT
public:
    ConnectionPrivate(Connection* parent)
        : QObject(parent), client(0), conn(parent), pendingRead(0), pendingWrite(0), done(false)
    {
    }

    ~ConnectionPrivate()
    {
        delete client;
    }

public slots:
    void dataAvailable();
    void dataWritten(int bytes);

public:
    LocalClient* client;
    Connection* conn;
    int pendingRead, pendingWrite;
    bool done;
};

#include "Connection.cpp.moc"

void ConnectionPrivate::dataAvailable()
{
    forever {
        int available = client->bytesAvailable();
        if (!pendingRead) {
            if (available < static_cast<int>(sizeof(uint32_t)))
                break;
            char buf[sizeof(uint32_t)];
            const int read = client->read(buf, 4);
            assert(read == 4);
            Deserializer strm(buf, read);
            strm >> pendingRead;
            available -= 4;
        }
        if (available < pendingRead)
            break;
        char buf[1024];
        char *buffer = buf;
        if (pendingRead > static_cast<int>(sizeof(buf))) {
            buffer = new char[pendingRead];
        }
        const int read = client->read(buffer, pendingRead);
        assert(read == pendingRead);
        Deserializer strm(buffer, read);
        int id;
        ByteArray payload;
        strm >> id >> payload;
        assert(id > 0 && Connection::sMetas.contains(id));

        Connection::Meta m = Connection::sMetas.value(id);
        QObject *newobj = m.meta->newInstance(Q_ARG(QObject*, 0));
        assert(newobj);
        m.meta->method(m.fromByteArrayId).invoke(newobj, Q_ARG(ByteArray, ByteArray(payload.constData(), payload.size())));
        emit conn->newMessage(qobject_cast<Message*>(newobj));

        pendingRead = 0;
    }
}

void ConnectionPrivate::dataWritten(int bytes)
{
    Q_ASSERT(pendingWrite >= bytes);
    pendingWrite -= bytes;
    if (!pendingWrite) {
        if (bytes)
            emit conn->sendComplete();
        if (done)
            client->disconnect();
    }
}

Connection::Connection(QObject* parent)
    : QObject(parent), mPriv(new ConnectionPrivate(this))
{
    mPriv->client = new LocalClient;
    mPriv->client->connected().connect(this, &Connection::connected);
    mPriv->client->disconnected().connect(this, &Connection::disconnected);
    mPriv->client->dataAvailable().connect(mPriv, &ConnectionPrivate::dataAvailable);
    mPriv->client->bytesWritten().connect(mPriv, &ConnectionPrivate::dataWritten);
}

Connection::Connection(LocalClient* client, QObject* parent)
    : QObject(parent), mPriv(new ConnectionPrivate(this))
{
    assert(client->isConnected());
    mPriv->client = client;
    mPriv->client->disconnected().connect(this, &Connection::disconnected);
    mPriv->client->dataAvailable().connect(mPriv, &ConnectionPrivate::dataAvailable);
    mPriv->client->bytesWritten().connect(mPriv, &ConnectionPrivate::dataWritten);
}


bool Connection::connectToServer(const ByteArray &name)
{
    return mPriv->client->connect(name, 1000);
}

void Connection::send(int id, const ByteArray &message)
{
    if (!mPriv->client->isConnected()) {
        ::error("Trying to send message to unconnected client (%d)", id);
        return;
    }

    ByteArray header, data;
    {
        {
            Serializer strm(data);
            strm << id << message.size();
            strm.write(message.constData(), message.size());
        }
        {
            Serializer strm(header);
            strm << data.size();
        }
    }
    mPriv->pendingWrite += (header.size() + data.size());
    mPriv->client->write(header);
    mPriv->client->write(data);
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
