#include "Project.h"
#include "Indexer.h"
#include "FileManager.h"
#include "Server.h"

Project::Project(const Path &path)
    : mPath(path)
{
}

void Project::init()
{
    assert(!isValid());
    fileManager.reset(new FileManager);
    fileManager->init(shared_from_this());
    unsigned flags = Indexer::None;
    const unsigned options = Server::instance()->options().options;
    if (options & Server::Validate)
        flags |= Indexer::Validate;
    if (options & Server::IgnorePrintfFixits)
        flags |= Indexer::IgnorePrintfFixits;
    indexer.reset(new Indexer(shared_from_this(), flags));
}

void Project::restore()
{
    Timer timer;
    Path path = mPath;
    RTags::encodePath(path);
    const Path p = ByteArray::format<128>("%s%s", Server::instance()->options().dataDir.constData(), path.constData());
    if (FILE *f = fopen(p.constData(), "r")) {
        Deserializer in(f);
        int version;
        in >> version;
        if (version == Server::DatabaseVersion) {
            int fs;
            in >> fs;
            if (fs != RTags::fileSize(f)) {
                error("%s seems to be corrupted, refusing to restore %s",
                      p.constData(), mPath.constData());
            } else {
                if (!restore(in)) {
                    error("Can't restore project %s", mPath.constData());
                } else if (!indexer->restore(in)) {
                    error("Can't restore project %s", mPath.constData());
                } else {
                    error("Restored project %s in %dms", mPath.constData(), timer.elapsed());
                }
            }
        }
        fclose(f);
    }
}

bool Project::isValid() const
{
    return indexer.get();
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

Scope<const UsrMap&> Project::lockUsrForRead(int maxTime)
{
    Scope<const UsrMap&> scope;
    if (mUsrLock.lockForRead(maxTime))
        scope.mData.reset(new Scope<const UsrMap&>::Data(mUsr, &mUsrLock));
    return scope;
}

Scope<UsrMap&> Project::lockUsrForWrite()
{
    Scope<UsrMap&> scope;
    mUsrLock.lockForWrite();
    scope.mData.reset(new Scope<UsrMap&>::Data(mUsr, &mUsrLock));
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

bool Project::isIndexed(uint32_t fileId) const
{
    if (indexer)
        return indexer->isIndexed(fileId);
    return false;
}

bool Project::save(Serializer &out)
{
    {
        Scope<const SymbolMap &> scope = lockSymbolsForRead();
        out << scope.data();
    }
    {
        Scope<const SymbolNameMap &> scope = lockSymbolNamesForRead();
        out << scope.data();
    }
    {
        Scope<const UsrMap &> scope = lockUsrForRead();
        out << scope.data();
    }

    return true;
}

bool Project::restore(Deserializer &in)
{
    {
        Scope<SymbolMap &> scope = lockSymbolsForWrite();
        in >> scope.data();
    }
    {
        Scope<SymbolNameMap &> scope = lockSymbolNamesForWrite();
        in >> scope.data();
    }
    {
        Scope<UsrMap &> scope = lockUsrForWrite();
        in >> scope.data();
    }

    return true;
}

void Project::unload()
{
    if (indexer) {
        indexer->abort();
        indexer.reset();
        fileManager.reset();
    }
}

bool Project::match(const Match &p)
{
    Path paths[] = { p.pattern(), p.pattern() };
    paths[1].resolve();
    const int count = paths[1] != paths[0] ? 2 : 1;
    Scope<const FilesMap&> files = lockFilesForRead();
    for (int i=0; i<count; ++i) {
        const Path &path = paths[i];
        if (files.data().contains(path) || p.match(mPath))
            return true;
        const uint32_t id = Location::fileId(path);
        if (isIndexed(id))
            return true;

    }
    return false;
}
