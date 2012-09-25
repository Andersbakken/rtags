#ifndef WAITCONDITION_H
#define WAITCONDITION_H

#include "Mutex.h"
#include "Log.h"
#include <errno.h>
#include <time.h>

class WaitCondition
{
public:
    WaitCondition() { pthread_cond_init(&mCond, NULL); }
    ~WaitCondition() { pthread_cond_destroy(&mCond); }

    bool wait(Mutex* mutex, int maxTime = 0)
    {
        int ret;
        if (maxTime > 0) {
            timeval now;
            gettimeofday(&now, 0);
            timespec timeout;
            memset(&timeout, 0, sizeof(timespec));
            timeout.tv_sec = now.tv_sec + (maxTime / 1000);
            timeout.tv_nsec = ((now.tv_usec * 1000) + maxTime % 1000) * 1000;
            ret = pthread_cond_timedwait(&mCond, &mutex->mMutex, &timeout);
        } else {
            ret = pthread_cond_wait(&mCond, &mutex->mMutex);
        }
        switch (ret) {
        case ETIMEDOUT:
            return false;
        case 0:
            return true;
        default:
            error("WaitCondition::wait() error: %s", strerror(ret));
            return false;
        }
    }
    void wakeOne()
    {
        pthread_cond_signal(&mCond);
    }
    void wakeAll()
    {
        pthread_cond_broadcast(&mCond);
    }

private:
    pthread_cond_t mCond;
};

#endif
