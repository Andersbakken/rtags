#ifndef WAITCONDITION_H
#define WAITCONDITION_H

#include "Mutex.h"

class WaitCondition
{
public:
    WaitCondition() { pthread_cond_init(&mCond, NULL); }
    ~WaitCondition() { pthread_cond_destroy(&mCond); }

    void wait(Mutex* mutex) { pthread_cond_wait(&mCond, &mutex->mMutex); }
    void wakeOne() { pthread_cond_signal(&mCond); }
    void wakeAll() { pthread_cond_broadcast(&mCond); }

private:
    pthread_cond_t mCond;
};

#endif
