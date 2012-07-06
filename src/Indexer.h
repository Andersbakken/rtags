#ifndef INDEXER_H
#define INDEXER_H

#include "RTags.h"
#include "CursorInfo.h"
#include "EventReceiver.h"
#include "ReadWriteLock.h"
#include "FileSystemWatcher.h"
#include "MutexLocker.h"
#include <clang-c/Index.h>
#include <Timer.h>

class IndexerJob;
class Indexer : public EventReceiver
{
public:
    Indexer();
    ~Indexer();

    void init(const Path &srcRoot, bool validate);

    int index(const Path &input, const List<ByteArray> &arguments, unsigned indexerJobFlags);

    void setPchDependencies(const Path &pchHeader, const Set<uint32_t> &deps);
    void addDependencies(const DependencyMap &hash);
    Set<uint32_t> dependencies(uint32_t fileId) const;
    Set<uint32_t> pchDependencies(const Path &pchHeader) const;
    Map<ByteArray, Location> pchUSRMap(const List<Path> &pchFiles) const;
    void setPchUSRMap(const Path &pch, const PchUSRMap &astMap);
    void abort();
    bool visitFile(uint32_t fileId, const Path &p, bool isPch);
    Set<uint32_t> visitedFiles() const { MutexLocker lock(&mVisitedFilesMutex); return mVisitedFiles; }
    ByteArray fixIts(const Path &path) const;
    ByteArray errors(const Path &path) const;
    void setDiagnostics(const Map<uint32_t, List<ByteArray> > &errors,
                        const Map<Location, std::pair<int, ByteArray> > &fixIts);
    void reindex(const ByteArray &pattern);
    void event(const Event *event);
    signalslot::Signal1<int> &indexingDone() { return mIndexingDone; }
    signalslot::Signal1<Indexer*> &jobsComplete() { return mJobsComplete; }
    void onDirectoryChanged(const Path &path);
    Path srcRoot() const { return mSrcRoot; }
private:
    void onJobFinished(IndexerJob *job);
    void commitDependencies(const DependencyMap &deps, bool sync);
    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };
    void initDB(InitMode forceDirty, const ByteArray &pattern = ByteArray());
    bool needsToWaitForPch(IndexerJob *job) const;
    void startJob(IndexerJob *job);

    mutable ReadWriteLock mPchUSRMapLock;
    Map<Path, PchUSRMap> mPchUSRMaps;

    mutable Mutex mVisitedFilesMutex;
    Set<uint32_t> mVisitedFiles;

    mutable ReadWriteLock mPchDependenciesLock;
    Map<Path, Set<uint32_t> > mPchDependencies;
    int mJobCounter;

    mutable Mutex mMutex;

    ByteArray mPath;
    Map<int, IndexerJob*> mJobs, mWaitingForPCH, mWaitingForAbort;

    bool mTimerRunning;
    Timer mTimer;

    Path mSrcRoot;
    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    Mutex mWatchedMutex;
    WatchedMap mWatched;

    Map<Location, std::pair<int, ByteArray> > mFixIts;
    Map<uint32_t, ByteArray> mErrors;
    mutable ReadWriteLock mFixItsAndErrorsLock;

    signalslot::Signal1<int> mIndexingDone;
    signalslot::Signal1<Indexer*> mJobsComplete;
};

inline bool Indexer::visitFile(uint32_t fileId, const Path &path, bool isPch)
{
    MutexLocker lock(&mVisitedFilesMutex);
    if (!isPch && mVisitedFiles.contains(fileId)) {
        return false;
    }
    mVisitedFiles.insert(fileId);
    return true;
}
#endif
