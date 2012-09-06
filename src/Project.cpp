#include "Project.h"
#include "Indexer.h"
#include "GRTags.h"
#include "Server.h"

Project::Project()
    : indexer(0), grtags(0)
{
}

Project::~Project()
{
    delete grtags;
    if (indexer)
        indexer->abort();
    delete indexer;
}

Scope<const SymbolMap&> Project::lockSymbolsForRead()
{
    mSymbolsLock.lockForRead();
    Scope<const SymbolMap&> scope;
    scope.mData.reset(new Scope<const SymbolMap&>::Data(mSymbols, &mSymbolsLock));
    return scope;
}

Scope<SymbolMap&> Project::lockSymbolsForWrite()
{
    mSymbolsLock.lockForWrite();
    Scope<SymbolMap&> scope;
    scope.mData.reset(new Scope<SymbolMap&>::Data(mSymbols, &mSymbolsLock));
    return scope;
}

Scope<const SymbolNameMap&> Project::lockSymbolNamesForRead()
{
    mSymbolNamesLock.lockForRead();
    Scope<const SymbolNameMap&> scope;
    scope.mData.reset(new Scope<const SymbolNameMap&>::Data(mSymbolNames, &mSymbolNamesLock));
    return scope;
}

Scope<SymbolNameMap&> Project::lockSymbolNamesForWrite()
{
    mSymbolNamesLock.lockForWrite();
    Scope<SymbolNameMap&> scope;
    scope.mData.reset(new Scope<SymbolNameMap&>::Data(mSymbolNames, &mSymbolNamesLock));
    return scope;
}

Scope<const SymbolNameMap&> Project::lockGRForRead()
{
    mGRLock.lockForRead();
    Scope<const SymbolNameMap&> scope;
    scope.mData.reset(new Scope<const SymbolNameMap&>::Data(mGR, &mGRLock));
    return scope;
}

Scope<SymbolNameMap&> Project::lockGRForWrite()
{
    mGRLock.lockForWrite();
    Scope<SymbolNameMap&> scope;
    scope.mData.reset(new Scope<SymbolNameMap&>::Data(mGR, &mGRLock));
    return scope;
}

Scope<const GRFilesMap&> Project::lockGRFilesForRead()
{
    mGRFilesLock.lockForRead();
    Scope<const GRFilesMap&> scope;
    scope.mData.reset(new Scope<const GRFilesMap&>::Data(mGRFiles, &mGRFilesLock));
    return scope;
}

Scope<GRFilesMap&> Project::lockGRFilesForWrite()
{
    mGRFilesLock.lockForWrite();
    Scope<GRFilesMap&> scope;
    scope.mData.reset(new Scope<GRFilesMap&>::Data(mGRFiles, &mGRFilesLock));
    return scope;
}

void Project::dirty(const Set<uint32_t> &fileIds)
{
    {
        Scope<SymbolMap&> symbols = lockSymbolsForWrite();
        RTags::dirtySymbols(symbols.t(), fileIds);
    }
    {
        Scope<SymbolNameMap&> symbolNames = lockSymbolNamesForWrite();
        RTags::dirtySymbolNames(symbolNames.t(), fileIds);
    }
}
