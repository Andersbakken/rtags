#include "Connection.h"
#include <LocalClient.h>
#include "Event.h"
#include "EventLoop.h"

class ResponseMessageEvent : public Event
{
public:
    enum { Type = 1 };
    ResponseMessageEvent(const ByteArray &r)
        : Event(Type), response(r)
    {}

    const ByteArray response;
};

Connection::Connection()
    : mClient(new LocalClient), mPendingRead(0), mPendingWrite(0), mDone(false)
{
    mClient->connected().connect(mConnected);
    mClient->disconnected().connect(mDisconnected);
    mClient->dataAvailable().connect(this, &Connection::dataAvailable);
    mClient->bytesWritten().connect(this, &Connection::dataWritten);
}

Connection::Connection(LocalClient* client)
    : mClient(client), mPendingRead(0), mPendingWrite(0), mDone(false)
{
    assert(client->isConnected());
    mClient->disconnected().connect(mDisconnected);
    mClient->dataAvailable().connect(this, &Connection::dataAvailable);
    mClient->bytesWritten().connect(this, &Connection::dataWritten);
}

Connection::~Connection()
{
    mDestroyed(this);
    delete mClient;
}


bool Connection::connectToServer(const ByteArray &name)
{
    return mClient->connect(name, 1000);
}

bool Connection::send(int id, const ByteArray &message)
{
    if (!mClient->isConnected()) {
        ::error("Trying to send message to unconnected client (%d)", id);
        return false;
    }

    ByteArray header, data;
    {
        {
            Serializer strm(data);
            strm << id;
            strm.write(message.constData(), message.size());
        }
        {
            Serializer strm(header);
            strm << data.size();
        }
    }
    mPendingWrite += (header.size() + data.size());
    return mClient->write(header) && mClient->write(data);
}

int Connection::pendingWrite() const
{
    return mPendingWrite;
}
void Connection::finish()
{
    mDone = true;
    dataWritten(0);
}

void Connection::dataAvailable()
{
    while (true) {
        int available = mClient->bytesAvailable();
        assert(available >= 0);
        if (!mPendingRead) {
            if (available < static_cast<int>(sizeof(uint32_t)))
                break;
            char buf[sizeof(uint32_t)];
            const int read = mClient->read(buf, 4);
            assert(read == 4);
            Deserializer strm(buf, read);
            strm >> mPendingRead;
            available -= 4;
        }
        if (available < mPendingRead)
            break;
        char buf[1024];
        char *buffer = buf;
        if (mPendingRead > static_cast<int>(sizeof(buf))) {
            buffer = new char[mPendingRead];
        }
        const int read = mClient->read(buffer, mPendingRead);
        assert(read == mPendingRead);
        Message *message = Messages::create(buffer, read);
        if (message) {
            newMessage()(message, this);
            delete message;
        }
        if (buffer != buf)
            delete[] buffer;

        mPendingRead = 0;
    }
}

void Connection::dataWritten(int bytes)
{
    assert(mPendingWrite >= bytes);
    mPendingWrite -= bytes;
    if (!mPendingWrite) {
        if (bytes)
            sendComplete()();
        if (mDone) {
            mClient->disconnect();
            deleteLater();
        }
    }
}

void Connection::write(const ByteArray &out)
{
    EventLoop::instance()->postEvent(this, new ResponseMessageEvent(out));
}

void Connection::event(const Event *e)
{
    switch (e->type()) {
    case ResponseMessageEvent::Type: {
        ResponseMessage msg(static_cast<const ResponseMessageEvent*>(e)->response);
        send(&msg);
        break; }
    default:
        EventReceiver::event(e);
    }
}
