#ifndef Project_h
#define Project_h

#include <tr1/memory>
#include "Path.h"
#include "RTags.h"
#include "ReadWriteLock.h"

template <typename T>
class Scope
{
public:
    T data() { return mData->t; }
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

class Indexer;
class FileManager;
class GRTags;
class Project
{
public:
    Project(const Path &src);
    ~Project();

    Indexer *indexer;
    FileManager *fileManager;
    GRTags *grtags;

    const Path srcRoot;

    Scope<const SymbolMap&> lockSymbolsForRead();
    Scope<SymbolMap&> lockSymbolsForWrite();
    Scope<const SymbolNameMap&> lockSymbolNamesForRead();
    Scope<SymbolNameMap&> lockSymbolNamesForWrite();

    Scope<const GRMap&> lockGRForRead();
    Scope<GRMap&> lockGRForWrite();

    Scope<const FilesMap&> lockFilesForRead();
    Scope<FilesMap&> lockFilesForWrite();
private:
    SymbolMap mSymbols;
    ReadWriteLock mSymbolsLock;

    SymbolNameMap mSymbolNames;
    ReadWriteLock mSymbolNamesLock;

    FilesMap mFiles;
    ReadWriteLock mFilesLock;

    GRMap mGR;
    ReadWriteLock mGRLock;
};

#endif
