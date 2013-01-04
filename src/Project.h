#ifndef Project_h
#define Project_h

#include "CursorInfo.h"
#include "Path.h"
#include "RTags.h"
#include "Match.h"
#include "RegExp.h"
#include "ReadWriteLock.h"

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
    shared_ptr<Data> mData;
};

class Indexer;
class FileManager;
class Project : public enable_shared_from_this<Project>
{
public:
    Project(const Path &path);
    bool isValid() const;
    void init();
    void restore();

    void unload();

    shared_ptr<Indexer> indexer;
    shared_ptr<FileManager> fileManager;

    Path path() const { return mPath; }

    bool match(const Match &match);

    Scope<const SymbolMap&> lockSymbolsForRead(int maxTime = 0);
    Scope<SymbolMap&> lockSymbolsForWrite();

    Scope<const SymbolNameMap&> lockSymbolNamesForRead(int maxTime = 0);
    Scope<SymbolNameMap&> lockSymbolNamesForWrite();

    Scope<const FilesMap&> lockFilesForRead(int maxTime = 0);
    Scope<FilesMap&> lockFilesForWrite();

    Scope<const UsrMap&> lockUsrForRead(int maxTime = 0);
    Scope<UsrMap&> lockUsrForWrite();

    bool isIndexed(uint32_t fileId) const;

    bool save(Serializer &out);
    bool restore(Deserializer &in);
private:
    const Path mPath;

    SymbolMap mSymbols;
    ReadWriteLock mSymbolsLock;

    SymbolNameMap mSymbolNames;
    ReadWriteLock mSymbolNamesLock;

    UsrMap mUsr;
    ReadWriteLock mUsrLock;

    FilesMap mFiles;
    ReadWriteLock mFilesLock;
};

#endif
