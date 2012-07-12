#ifndef DirtyThread_h
#define DirtyThread_h

#include "Thread.h"
#include "Set.h"
#include "ScopedDB.h"
#include <stdint.h>

class DirtyThread : public Thread
{
public:
    DirtyThread(const Set<uint32_t> &dirtyFileIds, ScopedDB symbols, ScopedDB symbolNames);
protected:
    virtual void run();
private:
    const Set<uint32_t> mDirtyFileIds;
    ScopedDB mSymbols, mSymbolNames;
};

#endif
