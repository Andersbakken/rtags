#ifndef DirtyJob_h
#define DirtyJob_h

#include "ThreadPool.h"
#include "AbortInterface.h"
#include "Set.h"
#include "ScopedDB.h"
#include <stdint.h>

class DirtyJob : public ThreadPool::Job, public AbortInterface
{
public:
    enum { Priority = ThreadPool::Guaranteed };
    DirtyJob(const Set<uint32_t> &dirtyFileIds, ScopedDB symbols, ScopedDB symbolNames);
protected:
    virtual void run();
private:
    const Set<uint32_t> mDirtyFileIds;
    ScopedDB mSymbols, mSymbolNames;
};

#endif
