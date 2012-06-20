#ifndef MUTEXLOCKER_H
#define MUTEXLOCKER_H

#include "Mutex.h"

class MutexLocker
{
public:
    MutexLocker(Mutex* mutex)
        : mMutex(mutex), mLocked(true)
    {
        mMutex->lock();
    }
    ~MutexLocker() { if (mLocked) mMutex->unlock(); }

    bool isLocked() const { return mLocked; }
    void unlock() { if (mLocked) { mMutex->unlock(); mLocked = false; } }
    void relock() { if (!mLocked) { mMutex->lock(); mLocked = true; } }

private:
    Mutex* mMutex;
    bool mLocked;
};

#endif
