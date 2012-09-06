#ifndef INDEXER_H
#define INDEXER_H

#include "CursorInfo.h"
#include "FileSystemWatcher.h"
#include "FileInformation.h"
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
    Indexer(const shared_ptr<Project> &project, bool validate);

    typedef Map<Path, List<ByteArray> > PendingMap; // without this clang 3.1 complains
    void index(const Path &input, const List<ByteArray> &arguments, unsigned indexerJobFlags,
               const Set<uint32_t> &dirtyFiles = Set<uint32_t>(),
               const PendingMap &pending = PendingMap());
    void addFileInformation(uint32_t fileId, const List<ByteArray> &args, time_t time);
    FileInformation fileInformation(uint32_t fileId) const;
    void addDependencies(const DependencyMap &hash);
    Set<uint32_t> dependencies(uint32_t fileId) const;
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
private:
    void onFilesModifiedTimeout();
    static void onFilesModifiedTimeout(int id, void *userData)
    {
        EventLoop::instance()->removeTimer(id);
        static_cast<Indexer*>(userData)->onFilesModifiedTimeout();
    }
    void onValidateDBJobErrors(const Set<Location> &errors);
    void onJobFinished(IndexerJob *job);
    void dirty(const Set<uint32_t> &dirtyFileIds, const Map<Path, List<ByteArray> > &dirty);

    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };
    void initDB(InitMode forceDirty, const ByteArray &pattern = ByteArray());
    void startJob(IndexerJob *job);

    Set<uint32_t> mVisitedFiles;

    int mJobCounter;

    mutable Mutex mMutex;
    WaitCondition mWaitCondition;

    ByteArray mPath;
    Map<uint32_t, IndexerJob*> mJobs;

    Set<uint32_t> mModifiedFiles;
    int mModifiedFilesTimerId;

    bool mTimerRunning;
    Timer mTimer;

    weak_ptr<Project> mProject;
    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    FileInformationMap mFileInformations;

    Set<Path> mWatchedPaths;

    Map<Location, std::pair<int, ByteArray> > mFixIts;
    Map<uint32_t, ByteArray> mErrors;

    Set<Location> mPreviousErrors;

    signalslot::Signal1<Indexer*> mJobsComplete;
    bool mValidate;

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
