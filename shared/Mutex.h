#ifndef MUTEX_H
#define MUTEX_H

#include <pthread.h>

class Mutex
{
public:
    Mutex() { pthread_mutex_init(&mMutex, NULL); }
    ~Mutex() { pthread_mutex_destroy(&mMutex); }

    void lock() { pthread_mutex_lock(&mMutex); }
    void unlock() { pthread_mutex_unlock(&mMutex); }
    bool tryLock() { return !pthread_mutex_trylock(&mMutex); }

private:
    pthread_mutex_t mMutex;
};

#endif
