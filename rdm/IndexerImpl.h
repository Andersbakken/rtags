#ifndef IndexerImpl_h
#define IndexerImpl_h

#include <QtCore>
#include "Indexer.h"

class IndexerJob;
class IndexerSyncer;
class IndexerImpl
{
public:
    int jobCounter;

    Indexer* indexer;

    QMutex implMutex;
    QWaitCondition implCond;
    QSet<QByteArray> indexing;
    QSet<QByteArray> pchHeaderError;

    QByteArray path;
    int lastJobId;
    QHash<int, IndexerJob*> jobs;

    IndexerSyncer* syncer;

    bool timerRunning;
    QElapsedTimer timer;

    QList<QByteArray> defaultArgs;

    QFileSystemWatcher watcher;
    DependencyHash dependencies, pchDeps;
    mutable QReadWriteLock pchDependenciesLock;
    QMutex watchedMutex;
    WatchedHash watched;

    void setPchDependencies(const Path &pchHeader, const QSet<Path> &deps);
    QSet<Path> pchDependencies(const Path &pchHeader) const;
    void commitDependencies(const DependencyHash& deps, bool sync);
};

#endif
