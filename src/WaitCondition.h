#ifndef WAITCONDITION_H
#define WAITCONDITION_H

#include "Mutex.h"
#include "Log.h"
#include <errno.h>

class WaitCondition
{
public:
    WaitCondition() { pthread_cond_init(&mCond, NULL); }
    ~WaitCondition() { pthread_cond_destroy(&mCond); }

    void wait(Mutex* mutex)
    {
        const int ret = pthread_cond_wait(&mCond, &mutex->mMutex);
        if (ret) {
            error("WaitCondition::wait() error: %s", strerror(ret));
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
