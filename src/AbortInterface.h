#ifndef AbortInterface_h
#define AbortInterface_h

#include "Mutex.h"
#include "MutexLocker.h"

class AbortInterface
{
public:
    AbortInterface()
    {
        mAborted = false;
    }

    virtual ~AbortInterface()
    {}

    inline void abort()
    {
        MutexLocker lock(&mMutex);
        mAborted = true;
    }

    inline bool isAborted() const
    {
        MutexLocker lock(&mMutex);
        return mAborted;
    }
private:
    mutable Mutex mMutex;
    bool mAborted;
};

#endif
