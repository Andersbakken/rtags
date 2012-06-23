#include "LocalClient.h"
#include "EventLoop.h"
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <algorithm>

LocalClient::LocalClient(const ByteArray& name)
    : mBufferIdx(0), mConnected(false)
{
    mFd = ::socket(PF_UNIX, SOCK_STREAM, 0);
    if (mFd == -1)
        return;
        
    struct sockaddr_un address;
    memset(&address, 0, sizeof(struct sockaddr_un));
    address.sun_family = AF_UNIX;
    const int sz = std::min<int>(sizeof(address.sun_path) - 1, name.size());
    memcpy(address.sun_path, name.constData(), sz);
    address.sun_path[sz] = '\0';
    if (::connect(mFd, (struct sockaddr *)&address, sizeof(struct sockaddr_un)) == -1) {
        ::close(mFd);
        return;
    }

    mConnected = true;

    const int flags = fcntl(mFd, F_GETFL, 0);
    fcntl(mFd, F_SETFL, flags | O_NONBLOCK);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read | EventLoop::Write, dataReadyCallback, this);
}

LocalClient::LocalClient(int fd)
    : mFd(fd), mBufferIdx(0), mConnected(true)
{
    const int flags = fcntl(mFd, F_GETFL, 0);
    fcntl(mFd, F_SETFL, flags | O_NONBLOCK);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read | EventLoop::Write, dataReadyCallback, this);
}

LocalClient::~LocalClient()
{
    ::close(mFd);
}

void LocalClient::dataReadyCallback(int, unsigned int flags, void* userData)
{
    LocalClient* client = reinterpret_cast<LocalClient*>(userData);
    if (flags & EventLoop::Read)
        client->mDataReady();
    if (flags & EventLoop::Write)
        client->writeMore();
}

ByteArray LocalClient::readAll()
{
    ByteArray data;
    enum { BufSize = 1024 };

    char buf[BufSize];
    for (;;) {
        const int r = ::read(mFd, buf, BufSize);
        if (r == -1)
            break;
        data += ByteArray(buf, r);
    }

    return data;
}

void LocalClient::write(const ByteArray& data)
{
    mBuffers.push_back(data);
    writeMore();
}

void LocalClient::writeMore()
{
    for (;;) {
        if (mBuffers.empty())
            return;
        const ByteArray& front = mBuffers.front();
        const int w = ::write(mFd, &front[mBufferIdx], front.size() - mBufferIdx);
        if (w == -1) // check EWOULDBLOCK / EAGAIN?
            return;
        mBufferIdx += w;
        if (mBufferIdx == front.size()) {
            mBuffers.pop_front();
            mBufferIdx = 0;
            continue;
        }
    }
}
