#include "Project.h"
#include "Indexer.h"
#include "FileManager.h"
#include "GRTags.h"
#include "Server.h"

Project::Project(const Path &src)
    : srcRoot(src)
{
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

Scope<const FilesMap&> Project::lockFilesForRead()
{
    mFilesLock.lockForRead();
    Scope<const FilesMap&> scope;
    scope.mData.reset(new Scope<const FilesMap&>::Data(mFiles, &mFilesLock));
    return scope;
}

Scope<FilesMap&> Project::lockFilesForWrite()
{
    mFilesLock.lockForWrite();
    Scope<FilesMap&> scope;
    scope.mData.reset(new Scope<FilesMap&>::Data(mFiles, &mFilesLock));
    return scope;
}

Scope<const GRMap&> Project::lockGRForRead()
{
    mGRLock.lockForRead();
    Scope<const GRMap&> scope;
    scope.mData.reset(new Scope<const GRMap&>::Data(mGR, &mGRLock));
    return scope;
}

Scope<GRMap&> Project::lockGRForWrite()
{
    mGRLock.lockForWrite();
    Scope<GRMap&> scope;
    scope.mData.reset(new Scope<GRMap&>::Data(mGR, &mGRLock));
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
