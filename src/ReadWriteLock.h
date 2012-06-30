#ifndef READWRITELOCK_H
#define READWRITELOCK_H

#include "Mutex.h"
#include "WaitCondition.h"

class ReadWriteLock
{
public:
    ReadWriteLock();
    ~ReadWriteLock();

    enum LockType {
        Read,
        Write
    };

    void lockForRead() { lock(Read); }
    void lockForWrite() { lock(Write); }
    void lock(LockType type);

    bool tryLockForRead() { return tryLock(Read); }
    bool tryLockForWrite() { return tryLock(Write); }
    bool tryLock(LockType type);

    void unlock();
private:
    Mutex mMutex;
    WaitCondition mCond;
    int mCount;
    bool mWrite;
};

#endif
