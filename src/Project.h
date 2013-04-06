#ifndef Project_h
#define Project_h

#include "CursorInfo.h"
#include <rct/Path.h>
#include "RTags.h"
#include "Match.h"
#include <rct/RegExp.h>
#include <rct/EventReceiver.h>
#include <rct/ReadWriteLock.h>
#include <rct/FileSystemWatcher.h>
#include "IndexerJob.h"

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

struct CachedUnit
{
    CachedUnit()
        : next(0), unit(0), index(0)
    {}
    ~CachedUnit()
    {
        if (unit)
            clang_disposeTranslationUnit(unit);
        if (index)
            clang_disposeIndex(index);
    }
    CachedUnit *next;
    CXTranslationUnit unit;
    CXIndex index;
    Path path;
    List<String> arguments;
};

class FileManager;
class IndexerJob;
class TimerEvent;
struct IndexData;
class Project : public EventReceiver
{
public:
    Project(const Path &path);
    bool isValid() const;
    void init();
    bool restore();

    void unload();

    shared_ptr<FileManager> fileManager;

    Path path() const { return mPath; }

    bool match(const Match &match, bool *indexed = 0);

    Scope<const SymbolMap&> lockSymbolsForRead(int maxTime = 0);
    Scope<SymbolMap&> lockSymbolsForWrite();

    Scope<const ErrorSymbolMap&> lockErrorSymbolsForRead(int maxTime = 0);
    Scope<ErrorSymbolMap&> lockErrorSymbolsForWrite();

    Scope<const SymbolNameMap&> lockSymbolNamesForRead(int maxTime = 0);
    Scope<SymbolNameMap&> lockSymbolNamesForWrite();

    Scope<const FilesMap&> lockFilesForRead(int maxTime = 0);
    Scope<FilesMap&> lockFilesForWrite();

    Scope<const UsrMap&> lockUsrForRead(int maxTime = 0);
    Scope<UsrMap&> lockUsrForWrite();

    bool isIndexed(uint32_t fileId) const;

    void index(const SourceInformation &args, IndexerJob::Type type);
    bool index(const Path &sourceFile, const Path &compiler = Path(), const List<String> &args = List<String>());
    SourceInformationMap sourceInfos() const;
    SourceInformation sourceInfo(uint32_t fileId) const;
    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn // slow
    };
    Set<uint32_t> dependencies(uint32_t fileId, DependencyMode mode) const;
    bool visitFile(uint32_t fileId);
    String fixIts(uint32_t fileId) const;
    int reindex(const Match &match);
    int remove(const Match &match);
    void onJobFinished(const shared_ptr<IndexerJob> &job);
    SourceInformationMap sources() const;
    DependencyMap dependencies() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool fetchFromCache(const Path &path, List<String> &args, CXIndex &index, CXTranslationUnit &unit);
    void addToCache(const Path &path, const List<String> &args, CXIndex index, CXTranslationUnit unit);
    void timerEvent(TimerEvent *event);
    bool isIndexing() const { MutexLocker lock(&mMutex); return !mJobs.isEmpty(); }
    void onJSFilesAdded();
private:
    // template <typename T> static Scope<const T&> lockForRead(T &t, ReadWriteLock &lock)
    // {
    //     Scope<const T&> scope;
    //     lock.lockForRead();
    //     scope.mData.reset(new Scope<const T&>::Data(t, &lock));
    //     return scope;
    // }

    // template <typename T> static Scope<T&> lockForWrite(T &t, ReadWriteLock &lock)
    // {
    //     Scope<T&> scope;
    //     lock.lockForWrite();
    //     scope.mData.reset(new Scope<T&>::Data(t, &lock));
    //     return scope;
    // }

    bool initJobFromCache(const Path &path, const List<String> &args,
                          CXIndex &index, CXTranslationUnit &unit, List<String> *argsOut);
    void onFileModified(const Path &);
    void addDependencies(const DependencyMap &hash, Set<uint32_t> &newFiles);
    void addFixIts(const DependencyMap &dependencies, const FixItMap &fixIts);
    int syncDB();
    void startDirtyJobs();
    void addCachedUnit(const Path &path, const List<String> &args, CXIndex index, CXTranslationUnit unit);
    bool save();
    void onValidateDBJobErrors(const Set<Location> &errors);

    const Path mPath;

    SymbolMap mSymbols;
    ReadWriteLock mSymbolsLock;

    ErrorSymbolMap mErrorSymbols;
    ReadWriteLock mErrorSymbolsLock;

    SymbolNameMap mSymbolNames;
    ReadWriteLock mSymbolNamesLock;

    UsrMap mUsr;
    ReadWriteLock mUsrLock;

    FilesMap mFiles;
    ReadWriteLock mFilesLock;

    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };

    Set<uint32_t> mVisitedFiles;

    int mJobCounter;

    mutable Mutex mMutex;

    Map<uint32_t, shared_ptr<IndexerJob> > mJobs;
    struct PendingJob
    {
        SourceInformation source;
        IndexerJob::Type type;
    };
    Map<uint32_t, PendingJob> mPendingJobs;

    Set<uint32_t> mModifiedFiles;
    Timer mModifiedFilesTimer, mSaveTimer, mSyncTimer;

    bool mTimerRunning;
    StopWatch mTimer;

    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    SourceInformationMap mSources;

    Set<Path> mWatchedPaths;

    FixItMap mFixIts;

    Set<Location> mPreviousErrors;

    Map<uint32_t, shared_ptr<IndexData> > mPendingData;
    Set<uint32_t> mPendingDirtyFiles;

    CachedUnit *mFirstCachedUnit, *mLastCachedUnit;
    int mUnitCacheSize;
};

inline bool Project::visitFile(uint32_t fileId)
{
    MutexLocker lock(&mMutex);
    if (mVisitedFiles.contains(fileId)) {
        return false;
    }

    mVisitedFiles.insert(fileId);
    return true;
}

#endif
