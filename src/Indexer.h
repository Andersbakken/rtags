#ifndef INDEXER_H
#define INDEXER_H

#include "CursorInfo.h"
#include "FileSystemWatcher.h"
#include "EventReceiver.h"
#include "MutexLocker.h"
#include "RTags.h"
#include "ReadWriteLock.h"
#include "ThreadPool.h"
#include "StopWatch.h"
#include "Match.h"
#include "Project.h"
#include <clang-c/Index.h>

struct IndexData;
class IndexerJob;
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
    List<ByteArray> arguments;
};

class Indexer : public EventReceiver
{
public:
    enum Flag {
        None = 0x0,
        Validate = 0x1,
        IgnorePrintfFixits = 0x2
    };
    Indexer(const shared_ptr<Project> &project, unsigned flags);
    void abort();

    void index(const SourceInformation &args, unsigned indexerJobFlags);
    SourceInformation sourceInfo(uint32_t fileId) const;
    Set<uint32_t> dependencies(uint32_t fileId) const;
    bool visitFile(uint32_t fileId, const shared_ptr<IndexerJob> &job);
    ByteArray fixIts(uint32_t fileId) const;
    ByteArray diagnostics() const;
    int reindex(const Match &match);
    shared_ptr<Project> project() const { return mProject.lock(); }
    void onJobFinished(const shared_ptr<IndexerJob> &job);
    bool isIndexed(uint32_t fileId) const;
    SourceInformationMap sources() const;
    DependencyMap dependencies() const;
    bool restore(Deserializer &in);
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool fetchFromCache(const Path &path, List<ByteArray> &args, CXIndex &index, CXTranslationUnit &unit);
    void addToCache(const Path &path, const List<ByteArray> &args, CXIndex index, CXTranslationUnit unit);
    void timerEvent(TimerEvent *event);
private:
    bool initJobFromCache(const Path &path, const List<ByteArray> &args,
                          CXIndex &index, CXTranslationUnit &unit, List<ByteArray> *argsOut);
    void onFileModified(const Path &);
    void addDependencies(const DependencyMap &hash, Set<uint32_t> &newFiles);
    void addDiagnostics(const DependencyMap &dependencies, const DiagnosticsMap &diagnostics, const FixItMap &fixIts);
    void write();
    void onFilesModifiedTimeout();
    void addCachedUnit(const Path &path, const List<ByteArray> &args, CXIndex index, CXTranslationUnit unit);
    bool finish();
    void save();
    void onValidateDBJobErrors(const Set<Location> &errors);

    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };

    Map<shared_ptr<IndexerJob>, Set<uint32_t> > mVisitedFilesByJob;
    Set<uint32_t> mVisitedFiles;

    int mJobCounter;

    mutable Mutex mMutex;

    ByteArray mPath;
    Map<uint32_t, shared_ptr<IndexerJob> > mJobs;

    Set<uint32_t> mModifiedFiles;
    Timer mModifiedFilesTimer, mFinishedTimer;

    bool mTimerRunning;
    StopWatch mTimer;
    int mLastJobElapsed;

    weak_ptr<Project> mProject;
    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    SourceInformationMap mSources;

    Set<Path> mWatchedPaths;

    FixItMap mFixIts;
    DiagnosticsMap mDiagnostics;

    Set<Location> mPreviousErrors;

    unsigned mFlags;

    Map<uint32_t, shared_ptr<IndexData> > mPendingData;
    Set<uint32_t> mPendingDirtyFiles;

    CachedUnit *mFirstCachedUnit, *mLastCachedUnit;
    int mUnitCacheSize;
};

inline bool Indexer::visitFile(uint32_t fileId, const shared_ptr<IndexerJob> &job)
{
    MutexLocker lock(&mMutex);
    if (mVisitedFiles.contains(fileId)) {
        return false;
    }

    mVisitedFiles.insert(fileId);
    mVisitedFilesByJob[job].insert(fileId);
    return true;
}

#endif
