#ifndef INDEXER_H
#define INDEXER_H

#include <QtCore>
#include <AddMessage.h>
#include "Rdm.h"
#include "CursorInfo.h"
#include <clang-c/Index.h>

class IndexerJob;
class Indexer : public QObject
{
    Q_OBJECT;
public:

    Indexer(const QByteArray& path, QObject* parent = 0);
    ~Indexer();

    int index(const QByteArray& input, const QList<QByteArray>& arguments);

    void setDefaultArgs(const QList<QByteArray> &args);
    inline QList<QByteArray> defaultArgs() const { return mDefaultArgs; }
    void setPchDependencies(const Path &pchHeader, const QSet<quint32> &deps);
    void addDependencies(const DependencyHash &hash);
    QSet<quint32> pchDependencies(const Path &pchHeader) const;
    QHash<QByteArray, Location> pchUSRHash(const QList<Path> &pchFiles) const;
    void setPchUSRHash(const Path &pch, const PchUSRHash &astHash);
    Path path() const { return mPath; }
    void abort();
    QList<QByteArray> compileArgs(const Path &file) const;
    bool visitFile(quint32 fileId, const Path &p);
    void dirty(const QSet<quint32> &files);
signals:
    void indexingDone(int id);
    void jobsComplete();
private slots:
    void onJobComplete(int id, const Path& input, bool isPch, const QByteArray &msg);
    void onDirectoryChanged(const QString& path);
private:
    void commitDependencies(const DependencyHash& deps, bool sync);
    void initDB();
    bool needsToWaitForPch(IndexerJob *job) const;
    void startJob(int id, IndexerJob *job);

    mutable QReadWriteLock mPchUSRHashLock;
    QHash<Path, PchUSRHash> mPchUSRHashes;

    QMutex mVisitedFilesMutex;
    QSet<quint32> mVisitedFiles;

    QList<QByteArray> mDefaultArgs;
    mutable QReadWriteLock mPchDependenciesLock;
    QHash<Path, QSet<quint32> > mPchDependencies;
    int mJobCounter;

    QMutex mMutex;
    QSet<Path> mIndexing;

    QByteArray mPath;
    QHash<int, IndexerJob*> mJobs, mWaitingForPCH;

    bool mTimerRunning;
    QElapsedTimer mTimer;

    QFileSystemWatcher mWatcher;
    DependencyHash mDependencies;
    QMutex mWatchedMutex;
    WatchedHash mWatched;
};

inline bool Indexer::visitFile(quint32 fileId, const Path &path)
{
    QMutexLocker lock(&mVisitedFilesMutex);
    if (Location::path(fileId).endsWith("Foo.h") && path.endsWith("Foo.cpp")) {
        printf("[%s] %s:%d: if (Location::path(fileId).endsWith(\"Foo.h\") && path.endsWith(\"Foo.cpp\")) { [after]\n", __func__, __FILE__, __LINE__);
        return false;
    }
    if (mVisitedFiles.contains(fileId)) {
        return false;
    }
    mVisitedFiles.insert(fileId);
    return true;
}
#endif
