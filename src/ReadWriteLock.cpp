#include "ReadWriteLock.h"
#include "MutexLocker.h"
#include <assert.h>

ReadWriteLock::ReadWriteLock()
    : mCount(0), mWrite(false), mOwner(0)
{
}

ReadWriteLock::~ReadWriteLock()
{
}

void ReadWriteLock::lock(LockType type)
{
    MutexLocker locker(&mMutex);
    if (type == Read) {
        if (mWrite) {
            for (;;) {
                mCond.wait(&mMutex);
                if (!mWrite) {
                    ++mCount;
                    return;
                }
            }
        } else {
            ++mCount;
        }
    } else {
        for (;;) {
            if (!mCount) {
                assert(!mWrite);
                mCount = 1;
                mWrite = true;
                mOwner = pthread_self();
                return;
            }
            assert(!pthread_equal(mOwner, pthread_self()));
            mCond.wait(&mMutex);
        }
    }
}

void ReadWriteLock::unlock()
{
    MutexLocker locker(&mMutex);
    assert(mCount > 0);
    if (mCount > 1) {
        --mCount;
        assert(!mWrite);
        return;
    }
    --mCount;
    assert(!mCount);
    mWrite = false;
    mOwner = 0;
    mCond.wakeAll();
}

bool ReadWriteLock::tryLock(LockType type)
{
    MutexLocker locker(&mMutex);
    if (type == Read) {
        if (mWrite)
            return false;
        ++mCount;
        return true;
    } else {
        if (mCount > 0)
            return false;
        assert(!mWrite && !mCount);
        mCount = 1;
        mWrite = true;
        return true;
    }
}
