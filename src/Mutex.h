#ifndef MUTEX_H
#define MUTEX_H

#include <pthread.h>
#include <RTags.h>
#include <Timer.h>

class Mutex
{
public:
    Mutex() { pthread_mutex_init(&mMutex, NULL); }
    ~Mutex() { pthread_mutex_destroy(&mMutex); }

#ifdef RTAGS_DEBUG
    void lock(); // implemented in RTags.cpp
#else
    void lock() { pthread_mutex_lock(&mMutex); }
#endif
    void unlock() { pthread_mutex_unlock(&mMutex); }
    bool tryLock() { return !pthread_mutex_trylock(&mMutex); }

private:
    pthread_mutex_t mMutex;

    friend class WaitCondition;
};

#endif
