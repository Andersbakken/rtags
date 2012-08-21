#ifndef LOCALSERVER_H
#define LOCALSERVER_H

#include "Path.h"
#include <SignalSlot.h>
#include <deque>

class LocalClient;

class LocalServer
{
public:
    LocalServer();
    ~LocalServer();

    bool listen(const Path& path);

    LocalClient* nextClient();

    signalslot::Signal0& clientConnected() { return mClientConnected; }

private:
    static void listenCallback(int fd, unsigned int flags, void* userData);

private:
    int mFd;
    std::deque<int> mPendingClients;
    signalslot::Signal0 mClientConnected;
};

#endif
