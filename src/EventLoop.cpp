#include "EventLoop.h"
#include "Event.h"
#include "EventReceiver.h"
#include "MutexLocker.h"
#include <unistd.h>
#include <errno.h>
#include <sys/select.h>
#include <fcntl.h>
#include <algorithm>

EventLoop::EventLoop()
    : mQuit(false)
{
    ::pipe2(mEventPipe, O_NONBLOCK);
}

EventLoop::~EventLoop()
{
    ::close(mEventPipe[0]);
    ::close(mEventPipe[1]);
}

void EventLoop::addFileDescriptor(int fd, FdFunc callback, void* userData)
{
    FdData data = { fd, callback, userData };
    mFdData.push_back(data);
}

void EventLoop::removeFileDescriptor(int fd)
{
    for (std::vector<FdData>::iterator it = mFdData.begin();
         it != mFdData.end(); ++it) {
        if (it->fd == fd) {
            mFdData.erase(it);
            break;
        }
    }
}

void EventLoop::postEvent(EventReceiver* receiver, Event* event)
{
    {
        EventData data = { receiver, event };

        MutexLocker locker(&mMutex);
        mEvents.push_back(data);
    }
    const char c = '\1';
    int r;
    do {
        r = ::write(mEventPipe[1], &c, 1);
    } while (r == -1 && errno == EAGAIN);
}

void EventLoop::run()
{
    fd_set set;
    int max;
    for (;;) {
        FD_ZERO(&set);
        FD_SET(mEventPipe[0], &set);
        max = mEventPipe[0];
        for (std::vector<FdData>::const_iterator it = mFdData.begin();
             it != mFdData.end(); ++it) {
            FD_SET(it->fd, &set);
            max = std::max(max, it->fd);
        }
        int r = ::select(max + 1, &set, 0, 0, 0);
        if (r == -1) { // ow
            return;
        }
        if (FD_ISSET(mEventPipe[0], &set))
            handlePipe();
        for (std::vector<FdData>::const_iterator it = mFdData.begin();
             it != mFdData.end(); ++it) {
            if (FD_ISSET(it->fd, &set))
                it->callback(it->fd, it->userData);
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
            case '\1':
                sendPostedEvents();
                break;
            case 'q':
                mQuit = true;
                break;
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
