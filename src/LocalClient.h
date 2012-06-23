#ifndef LOCALCLIENT_H
#define LOCALCLIENT_H

#include "ByteArray.h"
#include <signalslot.h>
#include <deque>

class LocalClient
{
public:
    LocalClient(const ByteArray& name);
    LocalClient(int fd);
    virtual ~LocalClient();

    bool isConnected() const { return mConnected; }

    ByteArray readAll();
    void write(const ByteArray& data);

    signalslot::Signal0& dataReady() { return mDataReady; }

private:
    static void dataReadyCallback(int fd, unsigned int flags, void* userData);

    void writeMore();

private:
    int mFd;
    signalslot::Signal0 mDataReady;
    std::deque<ByteArray> mBuffers;
    int mBufferIdx;
    bool mConnected;
};

#endif
