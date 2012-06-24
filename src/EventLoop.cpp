#include "EventLoop.h"
#include "Event.h"
#include "EventReceiver.h"
#include "MutexLocker.h"
#include <unistd.h>
#include <errno.h>
#include <sys/select.h>
#include <fcntl.h>
#include <algorithm>

EventLoop* EventLoop::sInstance = 0;

EventLoop::EventLoop()
    : mQuit(false)
{
    if (!sInstance)
        sInstance = this;
    if (::pipe2(mEventPipe, O_NONBLOCK) == -1) // ### ??? !!!
        ;
}

EventLoop::~EventLoop()
{
    ::close(mEventPipe[0]);
    ::close(mEventPipe[1]);
}

EventLoop* EventLoop::instance()
{
    return sInstance;
}

void EventLoop::addFileDescriptor(int fd, unsigned int flags, FdFunc callback, void* userData)
{
    FdData data = { fd, flags, callback, userData };
    MutexLocker locker(&mMutex);
    mFdData.push_back(data);
    locker.unlock();

    const char c = 'a';
    int r;
    do {
        r = ::write(mEventPipe[1], &c, 1);
    } while (r == -1 && errno == EAGAIN);
}

void EventLoop::removeFileDescriptor(int fd)
{
    bool found = false;
    MutexLocker locker(&mMutex);
    for (std::vector<FdData>::iterator it = mFdData.begin();
         it != mFdData.end(); ++it) {
        if (it->fd == fd) {
            mFdData.erase(it);
            found = true;
            break;
        }
    }

    if (found) {
        const char c = 'r';
        int r;
        do {
            r = ::write(mEventPipe[1], &c, 1);
        } while (r == -1 && errno == EAGAIN);
        mCond.wait(&mMutex);
    }
}

void EventLoop::postEvent(EventReceiver* receiver, Event* event)
{
    {
        EventData data = { receiver, event };

        MutexLocker locker(&mMutex);
        mEvents.push_back(data);
    }
    const char c = 'e';
    int r;
    do {
        r = ::write(mEventPipe[1], &c, 1);
    } while (r == -1 && errno == EAGAIN);
}

void EventLoop::run()
{
    fd_set rset, wset;
    int max;
    for (;;) {
        FD_ZERO(&rset);
        FD_ZERO(&wset);
        FD_SET(mEventPipe[0], &rset);
        max = mEventPipe[0];
        {
            MutexLocker locker(&mMutex);
            for (std::vector<FdData>::const_iterator it = mFdData.begin();
                 it != mFdData.end(); ++it) {
                if (it->flags & Read)
                    FD_SET(it->fd, &rset);
                if (it->flags & Write)
                    FD_SET(it->fd, &wset);
                max = std::max(max, it->fd);
            }
        }
        int r = ::select(max + 1, &rset, &wset, 0, 0);
        if (r == -1) { // ow
            return;
        }
        if (FD_ISSET(mEventPipe[0], &rset))
            handlePipe();
        std::vector<FdData> fds;
        {
            MutexLocker locker(&mMutex);
            fds = mFdData;
        }
        for (std::vector<FdData>::const_iterator it = fds.begin();
             it != fds.end(); ++it) {
            if ((it->flags & Read) && FD_ISSET(it->fd, &rset)) {
                unsigned int flag = Read;
                if ((it->flags & Write) && FD_ISSET(it->fd, &wset))
                    flag |= Write;
                it->callback(it->fd, flag, it->userData);
            } else if ((it->flags & Write) && FD_ISSET(it->fd, &wset))
                it->callback(it->fd, Write, it->userData);
        }
        if (mQuit)
            break;
    }
}

void EventLoop::handlePipe()
{
    char c;
    for (;;) {
        const int r = ::read(mEventPipe[0], &c, 1);
        if (r == 1) {
            switch (c) {
            case 'e':
                sendPostedEvents();
                break;
            case 'q':
                mQuit = true;
                break;
            case 'a':
                break;
            case 'r': {
                MutexLocker locker(&mMutex);
                mCond.wakeAll();
                break; }
            }
        } else
            break;
    }
}

void EventLoop::sendPostedEvents()
{
    MutexLocker locker(&mMutex);
    while (!mEvents.empty()) {
        std::vector<EventData>::iterator first = mEvents.begin();
        EventData data = *first;
        mEvents.erase(first);
        locker.unlock();
        data.receiver->event(data.event);
        locker.relock();
    }
}

void EventLoop::exit()
{
    const char q = 'q';
    int r;
    do {
        r = ::write(mEventPipe[1], &q, 1);
    } while (r == -1 && errno == EAGAIN);
}
