#include "LocalClient.h"
#include "Event.h"
#include "EventLoop.h"
#include <unistd.h>
#include <Timer.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <algorithm>

class DelayedWriteEvent : public Event
{
public:
    enum { Type = 1 };
    DelayedWriteEvent(const ByteArray& d)
        : Event(Type), data(d)
    {}

    const ByteArray data;
};

LocalClient::LocalClient()
    : mFd(-1), mBufferIdx(0)
{
}

LocalClient::LocalClient(int fd)
    : mFd(fd), mBufferIdx(0), mReadBufferPos(0)
{
    const int flags = fcntl(mFd, F_GETFL, 0);
    fcntl(mFd, F_SETFL, flags | O_NONBLOCK);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read | EventLoop::Write, dataCallback, this);
}

LocalClient::~LocalClient()
{
    disconnect();
}

bool LocalClient::connect(const ByteArray& name, int maxTime)
{
    Timer timer;
    struct sockaddr_un address;
    memset(&address, 0, sizeof(struct sockaddr_un));
    address.sun_family = AF_UNIX;
    const int sz = std::min<int>(sizeof(address.sun_path) - 1, name.size());
    memcpy(address.sun_path, name.constData(), sz);
    address.sun_path[sz] = '\0';
    forever {
        mFd = ::socket(PF_UNIX, SOCK_STREAM, 0);
        if (mFd == -1)
            return false;
        if (!::connect(mFd, (struct sockaddr *)&address, sizeof(struct sockaddr_un)))
            break;
        ::close(mFd);
        mFd = -1;
        if (maxTime > 0 && timer.elapsed() >= maxTime)
            return false;
    }

    assert(mFd != -1);
    const int flags = fcntl(mFd, F_GETFL, 0);
    fcntl(mFd, F_SETFL, flags | O_NONBLOCK);
    EventLoop::instance()->addFileDescriptor(mFd, EventLoop::Read | EventLoop::Write, dataCallback, this);

    mConnected();
    return true;
}

void LocalClient::disconnect()
{
    if (mFd != -1) {
        ::close(mFd);
        EventLoop::instance()->removeFileDescriptor(mFd);
        mFd = -1;
        mDisconnected();
    }
}

void LocalClient::dataCallback(int, unsigned int flags, void* userData)
{
    LocalClient* client = reinterpret_cast<LocalClient*>(userData);
    if (flags & EventLoop::Read)
        client->readMore();
    if (flags & EventLoop::Write)
        client->writeMore();
}

ByteArray LocalClient::readAll()
{
    ByteArray buf;
    std::swap(buf, mReadBuffer);
    if (mReadBufferPos) {
        buf.remove(0, mReadBufferPos);
        mReadBufferPos = 0;
    }
    return buf;
}

int LocalClient::read(char *buf, int size)
{
    size = std::min(bytesAvailable(), size);
    if (size) {
        memcpy(buf, mReadBuffer.data() + mReadBufferPos, size);
        mReadBufferPos += size;
        if (mReadBuffer.size() == mReadBufferPos) {
            mReadBuffer.clear();
            mReadBufferPos = 0;
        }
    }
    return size;
}

void LocalClient::write(const ByteArray& data)
{
    if (pthread_equal(pthread_self(), EventLoop::instance()->thread())) {
        mBuffers.push_back(data);
        writeMore();
    } else {
        EventLoop::instance()->postEvent(this, new DelayedWriteEvent(data));
    }
}

void LocalClient::readMore()
{
    enum { BufSize = 1024, MaxBufferSize = 1024 * 1024 * 16 };

    char buf[BufSize];
    int read = 0;
    bool wasDisconnected = false;
    for (;;) {
        const int r = ::read(mFd, buf, BufSize);
        if (r == -1) {
            break;
        } else if (!r) {
            wasDisconnected = true;
            break;
        }
        read += r;
        mReadBuffer.resize(r + mReadBuffer.size());
        memcpy(mReadBuffer.data() + mReadBuffer.size() - r, buf, r);
        if (mReadBuffer.size() + r >= MaxBufferSize) {
            if (mReadBuffer.size() + r - mReadBufferPos < MaxBufferSize) {
                mReadBuffer.remove(0, mReadBufferPos);
                mReadBufferPos = 0;
            } else {
                error("Buffer exhausted (%d), dropping on the floor", mReadBuffer.size());
                mReadBuffer.clear();
            }
        }
    }

    if (read && !mReadBuffer.isEmpty())
        mDataAvailable();
    if (wasDisconnected)
        disconnect();
}

void LocalClient::writeMore()
{
    // printf("Writemore called %p %p\n", (void*)pthread_self(),
    //        (void*)EventLoop::instance()->thread());
    int written = 0;
    for (;;) {
        if (mBuffers.empty())
            break;
        const ByteArray& front = mBuffers.front();
        const int w = ::write(mFd, &front[mBufferIdx], front.size() - mBufferIdx);
        if (w == -1) // check EWOULDBLOCK / EAGAIN?
            break;
        written += w;
        mBufferIdx += w;
        if (mBufferIdx == front.size()) {
            assert(!mBuffers.empty());
            mBuffers.pop_front();
            mBufferIdx = 0;
            continue;
        }
    }
    if (written)
        mBytesWritten(written);
}

void LocalClient::event(const Event* event)
{
    if (event->type() == DelayedWriteEvent::Type) {
        const DelayedWriteEvent *ev = static_cast<const DelayedWriteEvent*>(event);
        assert(pthread_equal(pthread_self(), EventLoop::instance()->thread()));
        mBuffers.push_back(ev->data);
        writeMore();
    } else {
        assert(0);
    }
}
