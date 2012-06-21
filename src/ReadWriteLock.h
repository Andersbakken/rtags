#ifndef READWRITELOCK_H
#define READWRITELOCK_H

#include "Mutex.h"
#include "WaitCondition.h"

class ReadWriteLock
{
public:
    ReadWriteLock();
    ~ReadWriteLock();

    void lockForRead();
    void lockForWrite();

    bool tryLockForRead();
    bool tryLockForWrite();

    void unlock();

private:
    Mutex mMutex;
    WaitCondition mCond;
    int mCount;
    bool mWrite;
};

#endif
