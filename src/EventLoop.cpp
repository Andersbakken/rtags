#include "EventLoop.h"
#include "Event.h"
#include "EventReceiver.h"
#include "MutexLocker.h"
#include "RTags.h"
#include "ThreadLocal.h"
#include "config.h"
#include <algorithm>
#include <errno.h>
#include <fcntl.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>
#include <sched.h>

#ifdef HAVE_MACH_ABSOLUTE_TIME
#include <mach/mach.h>
#include <mach/mach_time.h>
#endif

#define MAX_USEC 1000000

static inline bool timevalGreaterEqualThan(const timeval* a, const timeval* b)
{
    return (a->tv_sec > b->tv_sec
            || (a->tv_sec == b->tv_sec && a->tv_usec >= b->tv_usec));
}

static inline void timevalAdd(timeval* a, int diff)
{
    a->tv_sec += diff / 1000;
    a->tv_usec += (diff % 1000) * 1000;
    if (a->tv_usec >= MAX_USEC) {
        ++a->tv_sec;
        a->tv_usec -= MAX_USEC;
    }
}

static inline void timevalSub(timeval* a, timeval* b)
{
    a->tv_sec -= b->tv_sec;
    a->tv_usec -= b->tv_usec;
    if (a->tv_sec < 0) {
        a->tv_sec = a->tv_usec = 0;
    } else if (a->tv_usec < 0) {
        if (--a->tv_sec < 0) {
            a->tv_sec = a->tv_usec = 0;
        } else {
            a->tv_usec += MAX_USEC;
        }
    }
}

static inline uint64_t timevalMs(timeval* a)
{
    return (a->tv_sec * 1000LLU) + (a->tv_usec / 1000LLU);
}

static inline int timevalDiff(timeval* a, timeval* b)
{
    const uint64_t ams = timevalMs(a);
    const uint64_t bms = timevalMs(b);
    return ams - bms;
}

static bool gettime(timeval* time)
{
#if defined(HAVE_MACH_ABSOLUTE_TIME)
    static mach_timebase_info_data_t info;
    static bool first = true;
    uint64_t machtime = mach_absolute_time();
    if (first) {
        first = false;
        mach_timebase_info(&info);
    }
    machtime = machtime * info.numer / (info.denom * 1000); // microseconds
    time->tv_sec = machtime / 1000000;
    time->tv_usec = machtime % 1000000;
#elif defined(HAVE_CLOCK_MONOTONIC_RAW) || defined(HAVE_CLOCK_MONOTONIC)
    timespec spec;
#if defined(HAVE_CLOCK_MONOTONIC_RAW)
    const clockid_t cid = CLOCK_MONOTONIC_RAW;
#else
    const clockid_t cid = CLOCK_MONOTONIC;
#endif
    const int ret = ::clock_gettime(cid, &spec);
    if (ret == -1) {
        memset(time, 0, sizeof(timeval));
        return false;
    }
    time->tv_sec = spec.tv_sec;
    time->tv_usec = spec.tv_nsec / 1000;
#else
#error No EventLoop::gettime() implementation
#endif
    return true;
}

EventLoop* EventLoop::sInstance = 0;

EventLoop::EventLoop()
    : mQuit(false), mNextTimerHandle(0), mThread(0)
{
    if (!sInstance)
        sInstance = this;
    int flg;
    eintrwrap(flg, ::pipe(mEventPipe));
    eintrwrap(flg, ::fcntl(mEventPipe[0], F_GETFL, 0));
    eintrwrap(flg, ::fcntl(mEventPipe[0], F_SETFL, flg | O_NONBLOCK));
    eintrwrap(flg, ::fcntl(mEventPipe[1], F_GETFL, 0));
    eintrwrap(flg, ::fcntl(mEventPipe[1], F_SETFL, flg | O_NONBLOCK));
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
    return !timevalGreaterEqualThan(&a->when, &b->when);
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
    gettime(&data->when);
    timevalAdd(&data->when, timeout);
    mTimerByHandle[handle] = data;

    List<TimerData*>::iterator it = std::lower_bound(mTimerData.begin(), mTimerData.end(),
                                                     data, timerLessThan);
    mTimerData.insert(it, data);

    const char c = 't';
    int r;
    while (true) {
        r = ::write(mEventPipe[1], &c, 1);
        if (r != -1 || (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR)) {
            break;
        } else {
            sched_yield();
        }
    }

    return handle;
}

void EventLoop::removeTimer(int handle)
{
    MutexLocker locker(&mMutex);
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
    MutexLocker locker(&mMutex);
    FdData &data = mFdData[fd];
    data.flags = flags;
    data.callback = callback;
    data.userData = userData;
    if (!pthread_equal(pthread_self(), mThread)) {
        const char c = 'f';
        int r;
        while (true) {
            r = ::write(mEventPipe[1], &c, 1);
            if (r != -1 || (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR)) {
                break;
            } else {
                sched_yield();
            }
        }
    }
}

void EventLoop::removeFileDescriptor(int fd, unsigned int flags)
{
    MutexLocker locker(&mMutex);
    if (!flags)
        mFdData.remove(fd);
    else {
        Map<int, FdData>::iterator it = mFdData.find(fd);
        if (it == mFdData.end())
            return;
        it->second.flags &= ~flags;
        if (!it->second.flags)
            mFdData.erase(it);
    }
}

void EventLoop::removeEvents(EventReceiver *receiver)
{
    MutexLocker locker(&mMutex);
    std::deque<EventData>::iterator it = mEvents.begin();
    while (it != mEvents.end()) {
        if (it->receiver == receiver) {
            delete it->event;
            it = mEvents.erase(it);
        } else {
            ++it;
        }
    }
}

void EventLoop::postEvent(EventReceiver* receiver, Event* event)
{
    {
        assert(receiver);
        EventData data = { receiver, event };

        MutexLocker locker(&mMutex);
        mEvents.push_back(data);
    }
    if (!pthread_equal(pthread_self(), mThread)) {
        const char c = 'e';
        int r;
        while (true) {
            r = ::write(mEventPipe[1], &c, 1);
            if (r != -1 || (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR)) {
                break;
            } else {
                sched_yield();
            }
        }
    }
}

void EventLoop::run()
{
    mQuit = false;
    mThread = pthread_self();
    fd_set rset, wset;
    int max;
    timeval timedata, timenow;
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
        if (mTimerData.empty()) {
            timeout = 0;
        } else {
            gettime(&timenow);
            timedata = (*mTimerData.begin())->when;
            timevalSub(&timedata, &timenow);
            timeout = &timedata;
        }
        int r;
        // ### use poll instead? easier to catch exactly what fd that was problematic in the EBADF case
        eintrwrap(r, ::select(max + 1, &rset, &wset, 0, timeout));
        if (r == -1) { // ow
            error("Got error from select %d %s max %d used %d ", errno, strerror(errno), FD_SETSIZE, max + 1);
            return;
        }
        if (timeout) {
            gettime(&timenow);

            assert(mTimerData.begin() != mTimerData.end());
            List<TimerData> copy;
            {
                MutexLocker locker(&mMutex);
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
                    if (!timevalGreaterEqualThan(&timenow, &it->when))
                        break;
                    if (reinsertTimer(it->handle, &timenow))
                        it->callback(it->handle, it->userData);
                    MutexLocker locker(&mMutex);
                    while (true) {
                        ++it;
                        if (it == end || mTimerByHandle.contains(it->handle))
                            break;
                    }

                    if (it == end)
                        break;
                }
            }
        }
        if (FD_ISSET(mEventPipe[0], &rset))
            handlePipe();
        Map<int, FdData> fds;
        {
            MutexLocker locker(&mMutex);
            fds = mFdData;
        }

        Map<int, FdData>::const_iterator it = fds.begin();
        while (it != fds.end()) {
            if ((it->second.flags & (Read|Disconnected)) && FD_ISSET(it->first, &rset)) {
                unsigned int flag = it->second.flags & Read;
                if (it->second.flags & Disconnected) {
                    size_t nbytes = 0;
                    int ret = ioctl(it->first, FIONREAD, reinterpret_cast<char*>(&nbytes));
                    if (!ret && !nbytes) {
                        flag |= Disconnected;
                    }
                }

                if ((it->second.flags & Write) && FD_ISSET(it->first, &wset))
                    flag |= Write;
                it->second.callback(it->first, flag, it->second.userData);
            } else if ((it->second.flags & Write) && FD_ISSET(it->first, &wset)) {
                it->second.callback(it->first, Write, it->second.userData);
            } else {
                ++it;
                continue;
            }
            MutexLocker locker(&mMutex);
            do {
                ++it;
            } while (it != fds.end() && !mFdData.contains(it->first));
        }
        if (mQuit)
            break;
    }
}

bool EventLoop::reinsertTimer(int handle, timeval* now)
{
    MutexLocker locker(&mMutex);

    // first, find the handle in the list and remove it
    List<TimerData*>::iterator it = mTimerData.begin();
    const List<TimerData*>::const_iterator end = mTimerData.end();
    while (it != end) {
        if ((*it)->handle == handle) {
            TimerData* data = *it;
            mTimerData.erase(it);
            // how much over the target time are we?
            const int overtime = timevalDiff(now, &data->when);
            data->when = *now;
            // the next time we want to fire is now + timeout - overtime
            // but we don't want a negative time
            timevalAdd(&data->when, std::max(data->timeout - overtime, 0));
            // insert the time so that the list stays sorted by absolute time
            it = std::lower_bound(mTimerData.begin(), mTimerData.end(),
                                  data, timerLessThan);
            mTimerData.insert(it, data);
            // all done
            return true;
        }
        ++it;
    }
    // didn't find our timer handle in the list
    return false;
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
            case 't':
            case 'f':
            case 'q':
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
        std::deque<EventData>::iterator first = mEvents.begin();
        EventData data = *first;
        mEvents.erase(first);
        locker.unlock();
        data.receiver->sendEvent(data.event);
        delete data.event;
        locker.relock();
    }
}

void EventLoop::exit()
{
    MutexLocker lock(&mMutex);
    mQuit = true;
    if (!pthread_equal(pthread_self(), mThread)) {
        const char c = 'q';
        int r;
        while (true) {
            r = ::write(mEventPipe[1], &c, 1);
            if (r != -1 || (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR)) {
                break;
            } else {
                sched_yield();
            }
        }
    }
}
