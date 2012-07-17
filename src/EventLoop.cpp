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
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#ifdef HAVE_MACH_ABSOLUTE_TIME
#include <mach/mach.h>
#include <mach/mach_time.h>

static pthread_once_t sEventLoopInit = PTHREAD_ONCE_INIT;
static ThreadLocal<mach_timebase_info_data_t>* sTimebaseInfo;

static void initTimebaseInfo()
{
    // static leak
    sTimebaseInfo = new ThreadLocal<mach_timebase_info_data_t>();
}
#endif

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

static inline bool gettime(timeval* time)
{
#if defined(HAVE_MACH_ABSOLUTE_TIME)
    pthread_once(&sEventLoopInit, initTimebaseInfo);

    mach_timebase_info_data_t& info = sTimebaseInfo->get();
    uint64_t machtime = mach_absolute_time();
    if (info.denom == 0)
        (void)mach_timebase_info(&info);
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

static inline bool timevalGreaterEqualThan(timeval* a, timeval* b)
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

void EventLoop::run()
{
    mQuit = false;
    mThread = pthread_self();
    fd_set rset, wset;
    int max;
    timeval timedata, timeexpire;
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
            const int& next = (*mTimerData.begin())->timeout;
            timeout = &timedata;
            timedata.tv_sec = next / 1000;
            timedata.tv_usec = (next % 1000) * 1000;
            gettime(&timeexpire);
            timevalAdd(&timeexpire, next);
        }
        int r;
        // ### use poll instead? easier to catch exactly what fd that was problematic in the EBADF case
        eintrwrap(r, ::select(max + 1, &rset, &wset, 0, timeout));
        if (r == -1) { // ow
            return;
        }
        if (timeout) {
            timeval timenew;
            gettime(&timenew);
            if (timevalGreaterEqualThan(&timenew, &timeexpire)) {
                // the first timer has elapsed at the very least
                assert(mTimerData.begin() != mTimerData.end());
                int prevtimeout;
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
                        if (!timevalGreaterEqualThan(&timenew, &timeexpire))
                            break;
                        it->callback(it->handle, it->userData);
                        while (true) {
                            ++it;
                            if (it == end || mTimerByHandle.contains(it->handle))
                                break;
                        }

                        if (it == end)
                            break;
                        timevalAdd(&timeexpire, it->timeout - prevtimeout);
                    }
                }
            }
        }
        if (FD_ISSET(mEventPipe[0], &rset))
            handlePipe();
        Map<int, FdData> fds = mFdData;

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
