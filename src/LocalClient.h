#ifndef LOCALCLIENT_H
#define LOCALCLIENT_H

#include "ByteArray.h"
#include "EventReceiver.h"
#include <signalslot.h>
#include <deque>

class LocalClient : public EventReceiver
{
public:
    LocalClient();
    virtual ~LocalClient();

    bool connect(const ByteArray& name, int maxTime = -1);
    void disconnect();

    bool isConnected() const { return mFd != -1; }

    ByteArray readAll();
    int read(char *buf, int size);
    int bytesAvailable() const { return mReadBuffer.size() - mReadBufferPos; }
    void write(const ByteArray& data);

    signalslot::Signal0& dataAvailable() { return mDataAvailable; }
    signalslot::Signal0& connected() { return mConnected; }
    signalslot::Signal0& disconnected() { return mDisconnected; }
    signalslot::Signal1<int>& bytesWritten() { return mBytesWritten; }
protected:
    virtual void event(const Event* event);
private:
    static void dataCallback(int fd, unsigned int flags, void* userData);

    void writeMore();
    void readMore();
    LocalClient(int fd);
    friend class LocalServer;
    int mFd;
    signalslot::Signal0 mDataAvailable, mConnected, mDisconnected;
    signalslot::Signal1<int> mBytesWritten;

    std::deque<ByteArray> mBuffers;
    int mBufferIdx;
    ByteArray mReadBuffer;
    int mReadBufferPos;
};

#endif
