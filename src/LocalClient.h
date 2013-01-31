#ifndef LOCALCLIENT_H
#define LOCALCLIENT_H

#include <rct/String.h>
#include <rct/EventReceiver.h>
#include <rct/SignalSlot.h>
#include <deque>

class LocalClient : public EventReceiver
{
public:
    LocalClient();
    virtual ~LocalClient();

    bool connect(const Path& path, int maxTime = -1);
    void disconnect();

    bool isConnected() const { return mFd != -1; }

    String readAll();
    int read(char *buf, int size);
    int bytesAvailable() const { return mReadBuffer.size() - mReadBufferPos; }
    bool write(const String& data);

    signalslot::Signal1<LocalClient*> &dataAvailable() { return mDataAvailable; }
    signalslot::Signal1<LocalClient*> &connected() { return mConnected; }
    signalslot::Signal1<LocalClient*> &disconnected() { return mDisconnected; }
    signalslot::Signal2<LocalClient*, int>& bytesWritten() { return mBytesWritten; }
protected:
    virtual void event(const Event* event);
private:
    static void dataCallback(int fd, unsigned int flags, void* userData);

    bool writeMore();
    void readMore();
    LocalClient(int fd);
    friend class LocalServer;
    int mFd;
    signalslot::Signal1<LocalClient*> mDataAvailable, mConnected, mDisconnected;
    signalslot::Signal2<LocalClient*, int> mBytesWritten;

    std::deque<String> mBuffers;
    int mBufferIdx;
    String mReadBuffer;
    int mReadBufferPos;
};

#endif
