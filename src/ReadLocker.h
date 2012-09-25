#ifndef READLOCKER_H
#define READLOCKER_H

#include "ReadWriteLock.h"

class ReadLocker
{
public:
    ReadLocker(ReadWriteLock* lock)
        : mLock(lock)
    {
        if (mLock && !mLock->lockForRead())
            mLock = 0;
    }
    ~ReadLocker()
    {
        if (mLock)
            mLock->unlock();
    }

private:
    ReadWriteLock* mLock;
};

#endif
