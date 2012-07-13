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
    Mutex()
    {
        check(__FUNCTION__, pthread_mutex_init(&mMutex, 0));
    }
    ~Mutex()
    {
        check(__FUNCTION__, pthread_mutex_destroy(&mMutex));
    }

#ifdef RTAGS_DEBUG_MUTEX
    void lock(); // implemented in RTags.cpp
#else
    void lock()
    {
        check(__FUNCTION__, pthread_mutex_lock(&mMutex));
    }
#endif
    void unlock()
    {
        check(__FUNCTION__, pthread_mutex_unlock(&mMutex));
    }
    bool tryLock()
    {
        const int err = pthread_mutex_trylock(&mMutex);
        if (err == EBUSY) {
            return false;
        } else {
            check(__FUNCTION__, err);
        }
        return true;
    }

private:
    inline void check(const char *function, int err)
    {
        if (err) {
            error("Mutex tryLock failure %d %s", err, strerror(err));
            extern bool inSignalHandler;
            if (!inSignalHandler)
                assert(0);
        }
    }

    pthread_mutex_t mMutex;
    friend class WaitCondition;
};

#endif
