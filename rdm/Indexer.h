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

    int index(const Path &input, const QList<ByteArray> &arguments, unsigned indexerJobFlags);

    void setPchDependencies(const Path &pchHeader, const Set<quint32> &deps);
    void addDependencies(const DependencyHash &hash);
    Set<quint32> pchDependencies(const Path &pchHeader) const;
    Hash<ByteArray, Location> pchUSRHash(const QList<Path> &pchFiles) const;
    void setPchUSRHash(const Path &pch, const PchUSRHash &astHash);
    Path path() const { return mPath; }
    void abort();
    bool visitFile(quint32 fileId, const Path &p);
    Set<quint32> visitedFiles() const { MutexLocker lock(&mVisitedFilesMutex); return mVisitedFiles; }
    ByteArray fixIts(const Path &path) const;
    ByteArray errors(const Path &path) const;
    void setDiagnostics(const Hash<quint32, QList<ByteArray> > &errors,
                        const std::map<Location, QPair<int, ByteArray> > &fixIts);
    void reindex(const ByteArray &pattern);
signals:
    void indexingDone(int id);
    void jobsComplete();
private slots:
    void onJobComplete(int id, const Path &input, bool isPch, const ByteArray &msg);
    void onDirectoryChanged(const QString &path);
private:
    void commitDependencies(const DependencyHash &deps, bool sync);
    enum InitMode {
        Normal,
        ForceDirty
    };
    void initDB(InitMode forceDirty = Normal, const ByteArray &pattern = ByteArray());
    bool needsToWaitForPch(IndexerJob *job) const;
    void startJob(int id, IndexerJob *job);

    mutable QReadWriteLock mPchUSRHashLock;
    Hash<Path, PchUSRHash> mPchUSRHashes;

    mutable Mutex mVisitedFilesMutex;
    Set<quint32> mVisitedFiles;

    mutable QReadWriteLock mPchDependenciesLock;
    Hash<Path, Set<quint32> > mPchDependencies;
    int mJobCounter;

    Mutex mMutex;
    Set<Path> mIndexing;

    ByteArray mPath;
    Hash<int, IndexerJob*> mJobs, mWaitingForPCH;

    bool mTimerRunning;
    QElapsedTimer mTimer;

    QFileSystemWatcher mWatcher;
    DependencyHash mDependencies;
    Mutex mWatchedMutex;
    WatchedHash mWatched;

    std::map<Location, QPair<int, ByteArray> > mFixIts;
    Hash<quint32, ByteArray> mErrors;
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
