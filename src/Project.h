#ifndef Project_h
#define Project_h

#include <memory>
#include "Path.h"
#include "RTags.h"
#include "ReadWriteLock.h"
#include "CursorInfo.h"

template <typename T>
class Scope
{
public:
    bool isNull() const { return !mData; }
    bool isValid() const { return mData; }
    T data() const { return mData->t; }
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
    std::shared_ptr<Data> mData;
};

class Indexer;
class FileManager;
class GRTags;
class Project
{
public:
    Project(const Path &src);

    std::shared_ptr<Indexer> indexer;
    std::shared_ptr<FileManager> fileManager;
    std::shared_ptr<GRTags> grtags;

    const Path srcRoot;
    Path resolvedSrcRoot;

    Scope<const SymbolMap&> lockSymbolsForRead(int maxTime = 0);
    Scope<SymbolMap&> lockSymbolsForWrite();

    Scope<const SymbolNameMap&> lockSymbolNamesForRead(int maxTime = 0);
    Scope<SymbolNameMap&> lockSymbolNamesForWrite();

    Scope<const FilesMap&> lockFilesForRead(int maxTime = 0);
    Scope<FilesMap&> lockFilesForWrite();

    Scope<const UsrMap&> lockUsrForRead(int maxTime = 0);
    Scope<UsrMap&> lockUsrForWrite();

    Scope<const GRFilesMap&> lockGRFilesForRead(int maxTime = 0);
    Scope<GRFilesMap&> lockGRFilesForWrite();

    Scope<const GRMap&> lockGRForRead(int maxTime = 0);
    Scope<GRMap&> lockGRForWrite();

    bool isIndexed(uint32_t fileId) const;

    bool save(Serializer &out);
    bool restore(Deserializer &in);
private:
    SymbolMap mSymbols;
    ReadWriteLock mSymbolsLock;

    SymbolNameMap mSymbolNames;
    ReadWriteLock mSymbolNamesLock;

    UsrMap mUsr;
    ReadWriteLock mUsrLock;

    FilesMap mFiles;
    ReadWriteLock mFilesLock;

    GRFilesMap mGRFiles;
    ReadWriteLock mGRFilesLock;

    GRMap mGR;
    ReadWriteLock mGRLock;
};

#endif
