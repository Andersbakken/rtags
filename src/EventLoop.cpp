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
    MutexLocker locker(&mMutex);
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

    std::vector<TimerData*>::iterator it = std::lower_bound(mTimerData.begin(), mTimerData.end(),
                                                            data, timerLessThan);
    mTimerData.insert(it, data);

    return handle;
}

void EventLoop::removeTimer(int handle)
{
    MutexLocker locker(&mMutex);
    std::map<int, TimerData*>::iterator it = mTimerByHandle.find(handle);
    if (it == mTimerByHandle.end())
        return;
    TimerData* data = it->second;
    mTimerByHandle.erase(it);
    std::vector<TimerData*>::iterator dit = std::find(mTimerData.begin(), mTimerData.end(), data);
    assert(dit != mTimerData.end());
    assert((*dit)->handle == handle);
    mTimerData.erase(dit);
    delete data;
}

void EventLoop::addFileDescriptor(int fd, unsigned int flags, FdFunc callback, void* userData)
{
    FdData data = { fd, flags, callback, userData };
    MutexLocker locker(&mMutex);
    mFdData.push_back(data);
    locker.unlock();

    if (!pthread_equal(pthread_self(), mThread)) {
        const char c = 'a';
        int r;
        do {
            eintrwrap(r, ::write(mEventPipe[1], &c, 1));
        } while (r == -1 && errno == EAGAIN);
    }
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

    if (found && !pthread_equal(pthread_self(), mThread)) {
        const char c = 'r';
        int r;
        do {
            eintrwrap(r, ::write(mEventPipe[1], &c, 1));
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
        eintrwrap(r, ::write(mEventPipe[1], &c, 1));
    } while (r == -1 && errno == EAGAIN);
}

#define MAX_USEC 1000000

static inline bool gettime(timeval* time, int timeout)
{
    timespec spec;
    const int ret = ::clock_gettime(CLOCK_MONOTONIC_RAW, &spec);
    if (ret == -1)
        return false;
    time->tv_sec = spec.tv_sec;
    time->tv_usec = spec.tv_nsec / 1000;
    if (timeout != -1) {
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
    mThread = pthread_self();
    fd_set rset, wset;
    int max;
    timeval timedata;
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
            gettime(&newtime, -1);
            if (timevalGreaterEqualThan(&newtime, timeout)) {
                // the first timer has elapsed at the very least
                assert(mTimerData.begin() != mTimerData.end());
                int prevtimeout, diff;
                std::vector<TimerData> copy;
                {
                    MutexLocker locker(&mMutex);
                    std::vector<TimerData*>::const_iterator it = mTimerData.begin();
                    const std::vector<TimerData*>::const_iterator end = mTimerData.end();
                    while (it != end) {
                        copy.push_back(*(*it));
                        ++it;
                    }
                }
                // ### there's an issue here (and with the file descriptor code below as well)
                // where if a timer removes another timer and the user data is deleted then
                // this may cause a crash if that timer is also invoked as part of the same
                // select return
                std::vector<TimerData>::const_iterator it = copy.begin();
                const std::vector<TimerData>::const_iterator end = copy.end();
                while (it != end) {
                    prevtimeout = it->timeout;
                    if (timevalGreaterEqualThan(&newtime, timeout)) {
                        it->callback(it->handle, it->userData);
                    } else
                        break;
                    ++it;
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
        if (FD_ISSET(mEventPipe[0], &rset))
            handlePipe();
        std::vector<FdData> fds;
        {
            MutexLocker locker(&mMutex);
            fds = mFdData;
        }
        // ### there's an issue here (and with the timer code above as well) where if a fd callback
        // removes another fd and the user data is deleted then this may cause a crash if that
        // fd callback is also invoked as part of the same select return
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
    } while (r == -1 && errno == EAGAIN);
}
