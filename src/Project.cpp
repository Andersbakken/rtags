#include "Project.h"
#include "Indexer.h"
#include "GRTags.h"
#include "Server.h"

Project::Project()
    : indexer(0), grtags(0)
{
    memset(databases, 0, sizeof(databases));
}

Project::~Project()
{
    delete grtags;
    if (indexer)
        indexer->abort();
    delete indexer;
    for (int i=0; i<DatabaseTypeCount; ++i) {
        delete databases[i];
    }
}

ScopedDB Project::db(DatabaseType type, ReadWriteLock::LockType lockType) const
{
    assert(databases[type]);
    return ScopedDB(databases[type], lockType);
}

Path Project::databaseDir(DatabaseType type) const
{
    const char *dbNames[] = {
        "symbols.db",
        "symbolnames.db",
        "dependencies.db",
        "fileinfos.db",
        "grfiles.db",
        "gr.db"
    };

    char ret[PATH_MAX];
    const int w = snprintf(ret, sizeof(ret), "%s%s", projectPath.constData(), dbNames[type]);
    return Path(ret, w);
}

Scope<const SymbolMap&> Project::symbolsRead()
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

Scope<const SymbolNameMap&> Project::symbolNamesRead()
{
    mSymbolsLock.lockForRead();
    Scope<const SymbolNameMap&> scope;
    scope.mData.reset(new Scope<const SymbolNameMap&>::Data(mSymbolNames, &mSymbolNamesLock));
    return scope;
}

Scope<SymbolNameMap&> Project::lockSymbolNamesForWrite()
{
    mSymbolsLock.lockForWrite();
    Scope<SymbolNameMap&> scope;
    scope.mData.reset(new Scope<SymbolNameMap&>::Data(mSymbolNames, &mSymbolNamesLock));
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
