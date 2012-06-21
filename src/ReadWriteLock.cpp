#include "ReadWriteLock.h"
#include "MutexLocker.h"
#include <assert.h>

ReadWriteLock::ReadWriteLock()
    : mCount(0), mWrite(false)
{
}

ReadWriteLock::~ReadWriteLock()
{
}

void ReadWriteLock::lockForRead()
{
    MutexLocker locker(&mMutex);
    if (mWrite) {
        for (;;) {
            mCond.wait(&mMutex);
            if (!mWrite) {
                ++mCount;
                return;
            }
        }
    } else
        ++mCount;
}

void ReadWriteLock::lockForWrite()
{
    MutexLocker locker(&mMutex);
    for (;;) {
        if (!mCount) {
            mCount = 1;
            mWrite = true;
            return;
        }
        mCond.wait(&mMutex);
    }
}

bool ReadWriteLock::tryLockForRead()
{
    MutexLocker locker(&mMutex);
    if (mWrite)
        return false;
    ++mCount;
    return true;
}

bool ReadWriteLock::tryLockForWrite()
{
    MutexLocker locker(&mMutex);
    if (mCount > 0)
        return false;
    assert(!mWrite && !mCount);
    mCount = 1;
    mWrite = true;
    return true;
}

void ReadWriteLock::unlock()
{
    MutexLocker locker(&mMutex);
    assert(mCount > 0);
    if (mCount > 1) {
        --mCount;
        return;
    }
    --mCount;
    assert(!mCount);
    mWrite = false;
    mCond.wakeAll();
}
