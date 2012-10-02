#include "Project.h"
#include "Indexer.h"
#include "FileManager.h"
#include "GRTags.h"
#include "Server.h"

Project::Project(const Path &src)
    : srcRoot(src)
{
    resolvedSrcRoot = src;
    resolvedSrcRoot.resolve();
    if (resolvedSrcRoot == srcRoot)
        resolvedSrcRoot.clear();
}

Scope<const SymbolMap&> Project::lockSymbolsForRead(int maxTime)
{
    Scope<const SymbolMap&> scope;
    if (mSymbolsLock.lockForRead(maxTime))
        scope.mData.reset(new Scope<const SymbolMap&>::Data(mSymbols, &mSymbolsLock));
    return scope;
}

Scope<SymbolMap&> Project::lockSymbolsForWrite()
{
    Scope<SymbolMap&> scope;
    mSymbolsLock.lockForWrite();
    scope.mData.reset(new Scope<SymbolMap&>::Data(mSymbols, &mSymbolsLock));
    return scope;
}

Scope<const SymbolNameMap&> Project::lockSymbolNamesForRead(int maxTime)
{
    Scope<const SymbolNameMap&> scope;
    if (mSymbolNamesLock.lockForRead(maxTime))
        scope.mData.reset(new Scope<const SymbolNameMap&>::Data(mSymbolNames, &mSymbolNamesLock));
    return scope;
}

Scope<SymbolNameMap&> Project::lockSymbolNamesForWrite()
{
    Scope<SymbolNameMap&> scope;
    mSymbolNamesLock.lockForWrite();
    scope.mData.reset(new Scope<SymbolNameMap&>::Data(mSymbolNames, &mSymbolNamesLock));
    return scope;
}

Scope<const FilesMap&> Project::lockFilesForRead(int maxTime)
{
    Scope<const FilesMap&> scope;
    if (mFilesLock.lockForRead(maxTime))
        scope.mData.reset(new Scope<const FilesMap&>::Data(mFiles, &mFilesLock));
    return scope;
}

Scope<FilesMap&> Project::lockFilesForWrite()
{
    Scope<FilesMap&> scope;
    mFilesLock.lockForWrite();
    scope.mData.reset(new Scope<FilesMap&>::Data(mFiles, &mFilesLock));
    return scope;
}

Scope<const GRMap&> Project::lockGRForRead(int maxTime)
{
    Scope<const GRMap&> scope;
    if (mGRLock.lockForRead(maxTime))
        scope.mData.reset(new Scope<const GRMap&>::Data(mGR, &mGRLock));
    return scope;
}

Scope<GRMap&> Project::lockGRForWrite()
{
    Scope<GRMap&> scope;
    mGRLock.lockForWrite();
    scope.mData.reset(new Scope<GRMap&>::Data(mGR, &mGRLock));
    return scope;
}

Scope<const GRFilesMap&> Project::lockGRFilesForRead(int maxTime)
{
    Scope<const GRFilesMap&> scope;
    if (mGRFilesLock.lockForRead(maxTime))
        scope.mData.reset(new Scope<const GRFilesMap&>::Data(mGRFiles, &mGRFilesLock));
    return scope;
}

Scope<GRFilesMap&> Project::lockGRFilesForWrite()
{
    Scope<GRFilesMap&> scope;
    mGRFilesLock.lockForWrite();
    scope.mData.reset(new Scope<GRFilesMap&>::Data(mGRFiles, &mGRFilesLock));
    return scope;
}

bool Project::isIndexed(uint32_t fileId) const
{
    if (indexer)
        return indexer->isIndexed(fileId);
    if (grtags)
        return grtags->isIndexed(fileId);
    return false;
}
