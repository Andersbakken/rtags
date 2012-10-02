#ifndef CONNECTION_H
#define CONNECTION_H

#include "Messages.h"
#include "EventReceiver.h"
#include "LocalClient.h"
#include <ByteArray.h>
#include <Map.h>
#include <SignalSlot.h>

class ConnectionPrivate;
class LocalClient;
class Event;
class Connection : public EventReceiver
{
public:
    Connection();
    Connection(LocalClient *client);
    ~Connection();

    void setSilent(bool on) { mSilent = on; }
    bool isSilent() const { return mSilent; }

    bool connectToServer(const ByteArray &name);

    int pendingWrite() const;

    template<typename T> bool send(const T *message);
    bool send(int id, const ByteArray& message);
    template <int StaticBufSize>
    bool write(const char *format, ...)
    {
        va_list args;
        va_start(args, format);
        const ByteArray ret = ByteArray::snprintf<StaticBufSize>(format, args);
        va_end(args);
        ResponseMessage msg(ret);
        return send(&msg);
    }
    bool write(const ByteArray &out)
    {
        ResponseMessage msg(out);
        return send(&msg);
    }

    void writeAsync(const ByteArray &out);
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
    bool mDone, mSilent;

    signalslot::Signal0 mConnected, mDisconnected, mError, mSendComplete;
    signalslot::Signal2<Message*, Connection*> mNewMessage;
    signalslot::Signal1<Connection*> mDestroyed;
};

template<typename T>
bool Connection::send(const T *message)
{
    return send(message->messageId(), message->encode());
}

#endif // CONNECTION_H
