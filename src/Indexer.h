#ifndef INDEXER_H
#define INDEXER_H

#include "CursorInfo.h"
#include "FileSystemWatcher.h"
#include "MutexLocker.h"
#include "RTags.h"
#include "ReadWriteLock.h"
#include "ThreadPool.h"
#include "Timer.h"
#include "Project.h"
#include <clang-c/Index.h>

class IndexerJob;
class Indexer
{
public:
    Indexer();
    ~Indexer();

    void init(const shared_ptr<Project> &project, bool validate);

    void index(const Path &input, const List<ByteArray> &arguments, unsigned indexerJobFlags);
    void setPchDependencies(const Path &pchHeader, const Set<uint32_t> &deps);
    void addDependencies(const DependencyMap &hash);
    Set<uint32_t> dependencies(uint32_t fileId) const;
    Set<uint32_t> pchDependencies(const Path &pchHeader) const;
    void abort();
    bool visitFile(uint32_t fileId, const Path &p);
    bool isVisited(const Path &path) const;
    Set<uint32_t> visitedFiles() const { MutexLocker lock(&mMutex); return mVisitedFiles; }
    ByteArray fixIts(const Path &path) const;
    ByteArray errors(const Path &path) const;
    void setDiagnostics(const Map<uint32_t, List<ByteArray> > &errors,
                        const Map<Location, std::pair<int, ByteArray> > &fixIts);
    void reindex(const ByteArray &pattern);
    signalslot::Signal1<Indexer*> &jobsComplete() { return mJobsComplete; }
    void onFileModified(const Path &);
    shared_ptr<Project> project() const { return mProject.lock(); }
    Path srcRoot() const { return mProject.lock()->srcRoot; } // ~/src/foobar
    Path projectPath() const { return mProject.lock()->projectPath; } // ~/.rtags/projects/[_foobar_]
private:
    void onValidateDBJobErrors(const Set<Location> &errors);
    void onJobFinished(IndexerJob *job);
    void commitDependencies(const DependencyMap &deps, bool sync);
    void dirty(const Set<uint32_t> &dirtyFileIds,
               const Map<Path, List<ByteArray> > &dirtyPch,
               const Map<Path, List<ByteArray> > &dirty);

    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };
    void initDB(InitMode forceDirty, const ByteArray &pattern = ByteArray());
    bool needsToWaitForPch(IndexerJob *job) const;
    void startJob(IndexerJob *job);

    Set<uint32_t> mVisitedFiles;

    Map<Path, Set<uint32_t> > mPchDependencies;
    int mJobCounter;

    mutable Mutex mMutex;
    WaitCondition mWaitCondition;

    ByteArray mPath;
    Map<uint32_t, IndexerJob*> mJobs;
    enum WaitType {
        Abort,
        PCH
    };
    Map<uint32_t, std::pair<IndexerJob*, WaitType> > mWaiting;

    bool mTimerRunning;
    Timer mTimer;

    weak_ptr<Project> mProject;
    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;

    Set<Path> mWatchedPaths;

    Map<Location, std::pair<int, ByteArray> > mFixIts;
    Map<uint32_t, ByteArray> mErrors;

    Set<Location> mPreviousErrors;

    signalslot::Signal1<Indexer*> mJobsComplete;
};

inline bool Indexer::visitFile(uint32_t fileId, const Path &path)
{
    MutexLocker lock(&mMutex);
    if (mVisitedFiles.contains(fileId)) {
        return false;
    }

    mVisitedFiles.insert(fileId);
    return true;
}

inline bool Indexer::isVisited(const Path &path) const
{
    const uint32_t fileId = Location::insertFile(path);
    MutexLocker lock(&mMutex);
    return mVisitedFiles.contains(fileId);
}

#endif
