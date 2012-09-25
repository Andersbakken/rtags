#ifndef WRITELOCKER_H
#define WRITELOCKER_H

#include "ReadWriteLock.h"

class WriteLocker
{
public:
    WriteLocker(ReadWriteLock* lock)
        : mLock(lock)
    {
        if (mLock && !mLock->lockForWrite())
            mLock = 0;
    }
    ~WriteLocker()
    {
        if (mLock)
            mLock->unlock();
    }

private:
    ReadWriteLock* mLock;

};

#endif
