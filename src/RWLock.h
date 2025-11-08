/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef RWLock_h
#define RWLock_h

#include <shared_mutex>
#include <mutex>

class RWLock
{
public:
    RWLock() = default;

    void lockRead()
    {
        mMutex.lock_shared();
    }

    void unlockRead()
    {
        mMutex.unlock_shared();
    }

    void lockWrite()
    {
        mMutex.lock();
    }

    void unlockWrite()
    {
        mMutex.unlock();
    }

private:
    std::shared_mutex mMutex;
};

class ReadLocker
{
public:
    explicit ReadLocker(RWLock *lock) : mLock(lock)
    {
        if (mLock)
            mLock->lockRead();
    }

    ~ReadLocker()
    {
        if (mLock)
            mLock->unlockRead();
    }

    ReadLocker(const ReadLocker&) = delete;
    ReadLocker& operator=(const ReadLocker&) = delete;

private:
    RWLock *mLock;
};

class WriteLocker
{
public:
    explicit WriteLocker(RWLock *lock) : mLock(lock)
    {
        if (mLock)
            mLock->lockWrite();
    }

    ~WriteLocker()
    {
        if (mLock)
            mLock->unlockWrite();
    }

    WriteLocker(const WriteLocker&) = delete;
    WriteLocker& operator=(const WriteLocker&) = delete;

private:
    RWLock *mLock;
};

#endif
