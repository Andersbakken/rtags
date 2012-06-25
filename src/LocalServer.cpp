#include "LocalServer.h"
#include "LocalClient.h"
#include "EventLoop.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#define LISTEN_BACKLOG 5

LocalServer::LocalServer()
    : mFd(-1)
{
}

LocalServer::~LocalServer()
{
    if (mFd != -1) {
        EventLoop::instance()->removeFileDescriptor(mFd);
        ::close(mFd);
    }
}

bool LocalServer::listen(const ByteArray& name)
{
    struct sockaddr_un address;

    mFd = socket(PF_UNIX, SOCK_STREAM, 0);
    if (mFd < 0) {
        error("LocalServer::listen() Unable to create socket");
        return false;
    }

    unlink(name.constData());

    memset(&address, 0, sizeof(struct sockaddr_un));

    if (static_cast<int>(sizeof(address.sun_path)) - 1 <= name.size()) {
        error("LocalServer::listen() Path too long %s", name.constData());
        return false;
    }

    address.sun_family = AF_UNIX;
    memcpy(address.sun_path, name.nullTerminated(), name.size() + 1);

    if (bind(mFd, (struct sockaddr*)&address, sizeof(struct sockaddr_un)) != 0) {
        ::close(mFd);
        mFd = -1;
        error("LocalServer::listen() Unable to bind");
        return false;
    }

    if (::listen(mFd, LISTEN_BACKLOG) != 0) {
        ::close(mFd);
        error("LocalServer::listen() Unable to listen to socket");
        mFd = -1;
        return false;
    }

    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read, listenCallback, this);
    return true;
}

void LocalServer::listenCallback(int, unsigned int, void* userData)
{
    LocalServer* server = reinterpret_cast<LocalServer*>(userData);

    const int clientFd = ::accept(server->mFd, NULL, NULL);
    server->mPendingClients.push_back(clientFd);
    server->mClientConnected();
}

LocalClient* LocalServer::nextClient()
{
    if (mPendingClients.empty())
        return 0;
    const int clientFd = mPendingClients.front();
    mPendingClients.pop_front();
    return new LocalClient(clientFd);
}
