#ifndef INDEXER_H
#define INDEXER_H

#include <QtCore>
#include "Rdm.h"
#include "CursorInfo.h"
#include <clang-c/Index.h>

class IndexerJob;
class Indexer : public QObject
{
    Q_OBJECT;
public:

    Indexer(const ByteArray &path, QObject* parent = 0);
    ~Indexer();

    int index(const Path &input, const List<ByteArray> &arguments, unsigned indexerJobFlags);

    void setPchDependencies(const Path &pchHeader, const Set<quint32> &deps);
    void addDependencies(const DependencyMap &hash);
    Set<quint32> pchDependencies(const Path &pchHeader) const;
    Map<ByteArray, Location> pchUSRMap(const List<Path> &pchFiles) const;
    void setPchUSRMap(const Path &pch, const PchUSRMap &astMap);
    Path path() const { return mPath; }
    void abort();
    bool visitFile(quint32 fileId, const Path &p);
    Set<quint32> visitedFiles() const { MutexLocker lock(&mVisitedFilesMutex); return mVisitedFiles; }
    ByteArray fixIts(const Path &path) const;
    ByteArray errors(const Path &path) const;
    void setDiagnostics(const Map<quint32, List<ByteArray> > &errors,
                        const std::map<Location, QPair<int, ByteArray> > &fixIts);
    void reindex(const ByteArray &pattern);
signals:
    void indexingDone(int id);
    void jobsComplete();
private slots:
    void onJobComplete(int id, const Path &input, bool isPch, const ByteArray &msg);
    void onDirectoryChanged(const QString &path);
private:
    void commitDependencies(const DependencyMap &deps, bool sync);
    enum InitMode {
        Normal,
        ForceDirty
    };
    void initDB(InitMode forceDirty = Normal, const ByteArray &pattern = ByteArray());
    bool needsToWaitForPch(IndexerJob *job) const;
    void startJob(int id, IndexerJob *job);

    mutable QReadWriteLock mPchUSRMapLock;
    Map<Path, PchUSRMap> mPchUSRMapes;

    mutable Mutex mVisitedFilesMutex;
    Set<quint32> mVisitedFiles;

    mutable QReadWriteLock mPchDependenciesLock;
    Map<Path, Set<quint32> > mPchDependencies;
    int mJobCounter;

    Mutex mMutex;
    Set<Path> mIndexing;

    ByteArray mPath;
    Map<int, IndexerJob*> mJobs, mWaitingForPCH;

    bool mTimerRunning;
    QElapsedTimer mTimer;

    QFileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    Mutex mWatchedMutex;
    WatchedMap mWatched;

    std::map<Location, QPair<int, ByteArray> > mFixIts;
    Map<quint32, ByteArray> mErrors;
    mutable QReadWriteLock mFixItsAndErrorsLock;
};

inline bool Indexer::visitFile(quint32 fileId, const Path &path)
{
    MutexLocker lock(&mVisitedFilesMutex);
    if (mVisitedFiles.contains(fileId)) {
        return false;
    }
    mVisitedFiles.insert(fileId);
    return true;
}
#endif
