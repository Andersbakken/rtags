#ifndef WRITELOCKER_H
#define WRITELOCKER_H

#include "ReadWriteLock.h"

class WriteLocker
{
public:
    WriteLocker(ReadWriteLock* lock)
        : mLock(lock)
    {
        mLock->lockForWrite();
    }
    ~WriteLocker() { mLock->unlock(); }

private:
    ReadWriteLock* mLock;
};

#endif
