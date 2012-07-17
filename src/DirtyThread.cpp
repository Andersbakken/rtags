#include "DirtyThread.h"
#include "Database.h"
#include "RTags.h"
#include "Server.h"

DirtyThread::DirtyThread(const Set<uint32_t> &dirtyFileIds, ScopedDB symbols, ScopedDB symbolNames)
    : mDirtyFileIds(dirtyFileIds), mSymbols(symbols), mSymbolNames(symbolNames)
{
    setAutoDelete(true);
    assert(mSymbols.lockType() == ReadWriteLock::Write);
    assert(mSymbolNames.lockType() == ReadWriteLock::Write);
}

void DirtyThread::run()
{
    RTags::dirtySymbols(mSymbols, mDirtyFileIds);
    RTags::dirtySymbolNames(mSymbolNames, mDirtyFileIds);
}
