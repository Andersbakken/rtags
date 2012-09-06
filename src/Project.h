#ifndef Project_h
#define Project_h

#include <tr1/memory>
#include "Path.h"
#include "RTags.h"
#include "ReadWriteLock.h"

class Database;
class Indexer;
class GRTags;
class ScopedDB;
template <typename T>
class Scope
{
public:
    // T *operator->() { return mData->t; }
    T t() { return mData->t; }
private:
    friend class Project;
    struct Data {
        Data(T tt, ReadWriteLock *l)
            : t(tt), lock(l)
        {
        }
        ~Data()
        {
            lock->unlock();
        }
        T t;
        ReadWriteLock *lock;
    };
    shared_ptr<Data> mData;
};
class Project
{
public:
    enum DatabaseType {
        Symbol,
        SymbolName,
        Dependency,
        FileInformation,
        GRFiles,
        GR,
        DatabaseTypeCount
    };

    Project();
    ~Project();

    Database *databases[DatabaseTypeCount];
    Indexer *indexer;
    GRTags *grtags;

    Path srcRoot, projectPath;

    Path databaseDir(DatabaseType type) const;

    Scope<const SymbolMap&> symbolsRead();
    Scope<SymbolMap&> lockSymbolsForWrite();
    Scope<const SymbolNameMap&> symbolNamesRead();
    Scope<SymbolNameMap&> lockSymbolNamesForWrite();

    ScopedDB db(DatabaseType type, ReadWriteLock::LockType lockType) const;
    void dirty(const Set<uint32_t> &fileIds);
private:
    SymbolMap mSymbols;
    ReadWriteLock mSymbolsLock;
    SymbolNameMap mSymbolNames;
    ReadWriteLock mSymbolNamesLock;
};

#endif
