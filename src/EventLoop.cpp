#include "EventLoop.h"
#include "Event.h"
#include "EventReceiver.h"
#include "MutexLocker.h"
#include "Rdm.h"
#include <unistd.h>
#include <errno.h>
#include <sys/select.h>
#include <sys/time.h>
#include <time.h>
#include <fcntl.h>
#include <algorithm>

EventLoop* EventLoop::sInstance = 0;

EventLoop::EventLoop()
    : mQuit(false), mNextTimerHandle(0), mThread(0)
{
    if (!sInstance)
        sInstance = this;
    int err;
    eintrwrap(err, ::pipe2(mEventPipe, O_NONBLOCK));
}

EventLoop::~EventLoop()
{
    int err;
    eintrwrap(err, ::close(mEventPipe[0]));
    eintrwrap(err, ::close(mEventPipe[1]));
}

EventLoop* EventLoop::instance()
{
    return sInstance;
}

bool EventLoop::timerLessThan(TimerData* a, TimerData* b)
{
    return a->timeout < b->timeout;
}

int EventLoop::addTimer(int timeout, TimerFunc callback, void* userData)
{
    int handle = ++mNextTimerHandle;
    while (mTimerByHandle.find(handle) != mTimerByHandle.end()) {
        handle = ++mNextTimerHandle;
    }
    TimerData* data = new TimerData;
    data->handle = handle;
    data->timeout = timeout;
    data->callback = callback;
    data->userData = userData;
    mTimerByHandle[handle] = data;

    List<TimerData*>::iterator it = std::lower_bound(mTimerData.begin(), mTimerData.end(),
                                                     data, timerLessThan);
    mTimerData.insert(it, data);

    return handle;
}

void EventLoop::removeTimer(int handle)
{
    Map<int, TimerData*>::iterator it = mTimerByHandle.find(handle);
    if (it == mTimerByHandle.end())
        return;
    TimerData* data = it->second;
    mTimerByHandle.erase(it);
    List<TimerData*>::iterator dit = std::find(mTimerData.begin(), mTimerData.end(), data);
    assert(dit != mTimerData.end());
    assert((*dit)->handle == handle);
    mTimerData.erase(dit);
    delete data;
}

void EventLoop::addFileDescriptor(int fd, unsigned int flags, FdFunc callback, void* userData)
{
    FdData &data = mFdData[fd];
    data.flags = flags;
    data.callback = callback;
    data.userData = userData;
}

void EventLoop::removeFileDescriptor(int fd)
{
    mFdData.remove(fd);
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
        eintrwrap(r, ::write(mEventPipe[1], &c, 1));
    } while (r == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
}

#define MAX_USEC 1000000

static inline bool gettime(timeval* time, int timeout)
{
    timespec spec;
    const int ret = ::clock_gettime(CLOCK_MONOTONIC_RAW, &spec);
    if (ret == -1) {
        memset(time, 0, sizeof(timeval));
        return false;
    }
    time->tv_sec = spec.tv_sec;
    time->tv_usec = spec.tv_nsec / 1000;
    if (timeout) {
        time->tv_sec += timeout / 1000;
        time->tv_usec += (timeout % 1000) * 1000;
        if (time->tv_usec >= MAX_USEC) {
            ++time->tv_sec;
            time->tv_usec -= MAX_USEC;
        }
    }
    return true;
}

static inline bool timevalGreaterEqualThan(timeval* a, timeval* b)
{
    return (a->tv_sec > b->tv_sec
            || (a->tv_sec == b->tv_sec && a->tv_usec >= b->tv_usec));
}

void EventLoop::run()
{
    mQuit = false;
    mThread = pthread_self();
    fd_set rset, wset;
    int max;
    timeval timedata;
    for (;;) {
        FD_ZERO(&rset);
        FD_ZERO(&wset);
        FD_SET(mEventPipe[0], &rset);
        max = mEventPipe[0];
        for (Map<int, FdData>::const_iterator it = mFdData.begin();
             it != mFdData.end(); ++it) {
            if (it->second.flags & Read)
                FD_SET(it->first, &rset);
            if (it->second.flags & Write)
                FD_SET(it->first, &wset);
            max = std::max(max, it->first);
        }
        timeval* timeout;
        if (mTimerData.empty())
            timeout = 0;
        else {
            timeout = &timedata;
            gettime(timeout, (*mTimerData.begin())->timeout);
        }
        int r;
        // ### use poll instead? easier to catch exactly what fd that was problematic in the EBADF case
        eintrwrap(r, ::select(max + 1, &rset, &wset, 0, timeout));
        if (r == -1) { // ow
            return;
        }
        if (timeout) {
            timeval newtime;
            gettime(&newtime, 0);
            if (timevalGreaterEqualThan(&newtime, timeout)) {
                // the first timer has elapsed at the very least
                assert(mTimerData.begin() != mTimerData.end());
                int prevtimeout, diff;
                List<TimerData> copy;
                {
                    List<TimerData*>::const_iterator it = mTimerData.begin();
                    const List<TimerData*>::const_iterator end = mTimerData.end();
                    while (it != end) {
                        copy.push_back(*(*it));
                        ++it;
                    }
                }
                List<TimerData>::const_iterator it = copy.begin();
                const List<TimerData>::const_iterator end = copy.end();
                if (it != end) {
                    while (true) {
                        prevtimeout = it->timeout;
                        if (!timevalGreaterEqualThan(&newtime, timeout))
                            break;
                        it->callback(it->handle, it->userData);
                        while (true) {
                            ++it;
                            if (it == end || mTimerByHandle.contains(it->handle))
                                break;
                        }

                        if (it == end)
                            break;
                        diff = it->timeout - prevtimeout;
                        timeout->tv_sec += diff / 1000;
                        timeout->tv_usec += (diff % 1000) * 1000;
                        if (timeout->tv_usec >= MAX_USEC) {
                            ++timeout->tv_sec;
                            timeout->tv_usec -= MAX_USEC;
                        }
                    }
                }
            }
        }
        if (FD_ISSET(mEventPipe[0], &rset))
            handlePipe();
        Map<int, FdData> fds = mFdData;

        Map<int, FdData>::const_iterator it = fds.begin();
        while (it != fds.end()) {
            if ((it->second.flags & Read) && FD_ISSET(it->first, &rset)) {
                unsigned int flag = Read;
                if ((it->second.flags & Write) && FD_ISSET(it->first, &wset))
                    flag |= Write;
                it->second.callback(it->first, flag, it->second.userData);
            } else if ((it->second.flags & Write) && FD_ISSET(it->first, &wset)) {
                it->second.callback(it->first, Write, it->second.userData);
            } else {
                ++it;
                continue;
            }
            do {
                ++it;
            } while (it != fds.end() && !mFdData.contains(it->first));
        }
        if (mQuit)
            break;
    }
}

void EventLoop::handlePipe()
{
    char c;
    for (;;) {
        int r;
        eintrwrap(r, ::read(mEventPipe[0], &c, 1));
        if (r == 1) {
            switch (c) {
            case 'e':
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
        List<EventData>::iterator first = mEvents.begin();
        EventData data = *first;
        mEvents.erase(first);
        locker.unlock();
        data.receiver->event(data.event);
        delete data.event;
        locker.relock();
    }
}

void EventLoop::exit()
{
    const char q = 'q';
    int r;
    do {
        eintrwrap(r, ::write(mEventPipe[1], &q, 1));
    } while (r == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
}
