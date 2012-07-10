#ifndef MUTEX_H
#define MUTEX_H

#include "Log.h"
#include "Timer.h"
#include <assert.h>
#include <errno.h>
#include <pthread.h>

class Mutex
{
public:
    Mutex() { pthread_mutex_init(&mMutex, NULL); }
    ~Mutex() { pthread_mutex_destroy(&mMutex); }

#ifdef RTAGS_DEBUG_MUTEX
    void lock(); // implemented in RTags.cpp
#else
    void lock()
    {
        const int err = pthread_mutex_lock(&mMutex);
        if (err) {
            char buf[1024];
            strerror_r(err, buf, sizeof(buf));
            error("Mutex lock failure %d %s\n", err, buf);
            assert(0);
        }
    }
#endif
    void unlock()
    {
        const int err = pthread_mutex_unlock(&mMutex);
        if (err) {
            char buf[1024];
            strerror_r(err, buf, sizeof(buf));
            error("Mutex unlock failure %d %s\n", err, buf);
            assert(0);
        }
    }
    bool tryLock()
    {
        const int err = pthread_mutex_trylock(&mMutex);
        if (err == EBUSY) {
            return false;
        } else if (err) {
            char buf[1024];
            strerror_r(err, buf, sizeof(buf));
            error("Mutex tryLock failure %d %s\n", err, buf);
            assert(0);
            return false;
        }
        return true;
    }

private:
    pthread_mutex_t mMutex;

    friend class WaitCondition;
};

#endif
