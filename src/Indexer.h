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

struct IndexData;
class IndexerJob;
class Indexer : public enable_shared_from_this<Indexer>
{
public:
    Indexer(const shared_ptr<Project> &project, bool validate);

    void index(const SourceInformation &args, unsigned indexerJobFlags);
    SourceInformation sourceInfo(uint32_t fileId) const;
    Set<uint32_t> dependencies(uint32_t fileId) const;
    bool visitFile(uint32_t fileId, const shared_ptr<IndexerJob> &job);
    ByteArray fixIts(const Path &path) const;
    ByteArray errors(const Path &path = Path()) const;
    int reindex(const ByteArray &pattern, bool regexp);
    signalslot::Signal1<Indexer*> &jobsComplete() { return mJobsComplete; }
    signalslot::Signal2<Indexer*, const Path &> &jobComplete() { return mJobComplete; }
    signalslot::Signal2<Indexer*, const Path &> &jobStarted() { return mJobStarted; }
    shared_ptr<Project> project() const { return mProject.lock(); }
    void beginMakefile();
    void endMakefile();
    void onJobFinished(const shared_ptr<IndexerJob> &job);
    bool isIndexed(uint32_t fileId) const;
    SourceInformationMap sources() const;
    DependencyMap dependencies() const;
    bool save(Serializer &out);
    bool restore(Deserializer &in);
private:
    void checkFinished();
    void onFileModified(const Path &);
    void addDependencies(const DependencyMap &hash, Set<uint32_t> &newFiles);
    void addDiagnostics(const DiagnosticsMap &errors, const FixitMap &fixIts);
    void write();
    void onFilesModifiedTimeout();
    static void onFilesModifiedTimeout(int id, void *userData)
    {
        EventLoop::instance()->removeTimer(id);
        static_cast<Indexer*>(userData)->onFilesModifiedTimeout();
    }
    void onValidateDBJobErrors(const Set<Location> &errors);

    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };

    Map<shared_ptr<IndexerJob>, Set<uint32_t> > mVisitedFilesByJob;
    Set<uint32_t> mVisitedFiles;

    int mJobCounter;
    bool mInMakefile;

    mutable Mutex mMutex;

    ByteArray mPath;
    Map<uint32_t, shared_ptr<IndexerJob> > mJobs;

    Set<uint32_t> mModifiedFiles;
    int mModifiedFilesTimerId;

    bool mTimerRunning;
    Timer mTimer;

    weak_ptr<Project> mProject;
    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    SourceInformationMap mSources;

    Set<Path> mWatchedPaths;

    Map<Location, std::pair<int, ByteArray> > mFixIts;
    Map<uint32_t, ByteArray> mErrors;

    Set<Location> mPreviousErrors;

    signalslot::Signal1<Indexer*> mJobsComplete;
    signalslot::Signal2<Indexer*, const Path &> mJobComplete, mJobStarted;
    bool mValidate;

    Map<uint32_t, shared_ptr<IndexData> > mPendingData;
    Set<uint32_t> mPendingDirtyFiles;
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
