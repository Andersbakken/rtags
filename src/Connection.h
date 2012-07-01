#ifndef CONNECTION_H
#define CONNECTION_H

#include "Messages.h"
#include "EventReceiver.h"
#include "LocalClient.h"
#include <ByteArray.h>
#include <Map.h>
#include <signalslot.h>

class ConnectionPrivate;
class LocalClient;
class Event;
class Connection : public EventReceiver
{
public:
    Connection();
    Connection(LocalClient *client);
    ~Connection();

    bool connectToServer(const ByteArray &name);

    int pendingWrite() const;

    template<typename T>
    void send(const T *message);
    void send(int id, const ByteArray& message);
    void write(const ByteArray &out);
    void finish();

    bool isConnected() const { return mClient->isConnected(); }

    signalslot::Signal0 &connected() { return mConnected; }
    signalslot::Signal0 &disconnected() { return mDisconnected; }
    signalslot::Signal0 &error() { return mError; }
    signalslot::Signal2<Message*, Connection*> &newMessage() { return mNewMessage; }
    signalslot::Signal0 &sendComplete() { return mSendComplete; }
    signalslot::Signal1<Connection*> &destroyed() { return mDestroyed; }
protected:
    void event(const Event *e);
private:
    void dataAvailable();
    void dataWritten(int bytes);

    LocalClient *mClient;
    int mPendingRead, mPendingWrite;
    bool mDone;

    signalslot::Signal0 mConnected, mDisconnected, mError, mSendComplete;
    signalslot::Signal2<Message*, Connection*> mNewMessage;
    signalslot::Signal1<Connection*> mDestroyed;
};

template<typename T>
void Connection::send(const T *message)
{
    send(message->messageId(), message->encode());
}

#endif // CONNECTION_H
