#ifndef READLOCKER_H
#define READLOCKER_H

#include "ReadWriteLock.h"

class ReadLocker
{
public:
    ReadLocker(ReadWriteLock* lock)
        : mLock(lock)
    {
        mLock->lockForRead();
    }
    ~ReadLocker() { mLock->unlock(); }

private:
    ReadWriteLock* mLock;
};

#endif
