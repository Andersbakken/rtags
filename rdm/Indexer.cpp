#include "Server.h"
#include "DependencyEvent.h"
#include "DirtyJob.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "IndexerSyncer.h"
#include "Path.h"
#include "RTags.h"
#include "Rdm.h"
#include "SHA256.h"
#include "leveldb/write_batch.h"
#include <Log.h>
#include <QtCore>

Indexer::Indexer(const QByteArray& path, QObject* parent)
    : QObject(parent)
{
    qRegisterMetaType<Path>("Path");

    Q_ASSERT(path.startsWith('/'));
    if (!path.startsWith('/'))
        return;

    mJobCounter = 0;
    mPath = path + "pch/";
    Q_ASSERT(mPath.endsWith('/'));
    QDir dir;
    dir.mkpath(mPath);
    mTimerRunning = false;
    mSyncer = new IndexerSyncer(this);
    mSyncer->start();

    connect(&mWatcher, SIGNAL(directoryChanged(QString)),
            this, SLOT(onDirectoryChanged(QString)));

    leveldb::DB *db = Server::instance()->db(Server::PCH);
    const leveldb::ReadOptions readopts;
    leveldb::Iterator* it = db->NewIterator(readopts);
    it->SeekToFirst();
    while (it->Valid()) {
        if (it->key() == "dependencies") {
            mPchDependencies = Rdm::readValue<DependencyHash>(it);
        } else {
            mPchUSRHashes[it->key().data()] = Rdm::readValue<PchUSRHash>(it);
        }
        it->Next();
    }
    delete it;
    initWatcher();
    init();
}

void Indexer::initWatcher()
{
    leveldb::DB *db = Server::instance()->db(Server::Dependency);
    leveldb::Iterator* it = db->NewIterator(leveldb::ReadOptions());
    it->SeekToFirst();
    DependencyHash dependencies;
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        const Path file(key.data(), key.size());
        const QSet<Path> deps = Rdm::readValue<QSet<Path> >(it);
        dependencies[file] = deps;
        it->Next();
    }
    commitDependencies(dependencies, false);
    delete it;
}


static inline bool isDirty(const Path &path, const QSet<Path> &dependencies, quint64 time,
                           QSet<Path> &dirty)
{
    bool ret = (path.lastModified() > time);

    foreach(const Path &p, dependencies) {
        if (dirty.contains(p)) {
            ret = true;
        } else if (p.lastModified() > time) {
            dirty.insert(p);
            ret = true;
        }
    }
    verboseDebug() << "isDirty" << path << ret << path << QDateTime::fromTime_t(time) << dirty;
    return ret;
}

static inline bool isPch(const QList<QByteArray> &args)
{
    const int size = args.size();
    bool nextIsX = false;
    for (int i=0; i<size; ++i) {
        const QByteArray &arg = args.at(i);
        if (nextIsX) {
            return (arg == "c++-header" || arg == "c-header");
        } else if (arg == "-x") {
            nextIsX = true;
        } else if (arg.startsWith("-x")) {
            const QByteArray rest = QByteArray::fromRawData(arg.constData() + 2, arg.size() - 2);
            return (rest == "c++-header" || rest == "c-header");
        }
    }
    return false;
}

void Indexer::init()
{
    DependencyHash deps;
    leveldb::DB *fileInformationDB = Server::instance()->db(Server::FileInformation);
    leveldb::DB *dependencyDB = Server::instance()->db(Server::Dependency);
    leveldb::Iterator* it = dependencyDB->NewIterator(leveldb::ReadOptions());
    it->SeekToFirst();
    leveldb::WriteBatch batch;
    bool writeBatch = false;
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        const Path file(key.data(), key.size());
        if (file.isFile()) {
            foreach(const Path &p, Rdm::readValue<QSet<Path> >(it)) {
                deps[p].insert(file);
            }
        } else {
            batch.Delete(key);
            writeBatch = true;
        }
        it->Next();
    }
    delete it;
    if (writeBatch) {
        dependencyDB->Write(leveldb::WriteOptions(), &batch);
        writeBatch = false;
        batch = leveldb::WriteBatch();
    }

    QSet<Path> dirty;
    QHash<Path, QList<QByteArray> > toIndex, toIndexPch;

    it = fileInformationDB->NewIterator(leveldb::ReadOptions());
    it->SeekToFirst();
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        const Path path(key.data(), key.size());
        if (path.isFile()) {
            const FileInformation fi = Rdm::readValue<FileInformation>(it);
            if (!fi.compileArgs.isEmpty() && isDirty(path, deps.value(path), fi.lastTouched, dirty)) {
                // ### am I checking pch deps correctly here?
                if (isPch(fi.compileArgs)) {
                    toIndexPch[path] = fi.compileArgs;
                } else {
                    toIndex[path] = fi.compileArgs;
                }
            }
        } else {
            batch.Delete(key);
            writeBatch = true;
        }
        it->Next();
    }
    delete it;
    if (writeBatch)
        fileInformationDB->Write(leveldb::WriteOptions(), &batch);

    if (toIndex.isEmpty() && toIndexPch.isEmpty())
        return;

    QThreadPool::globalInstance()->start(new DirtyJob(this, dirty, toIndexPch, toIndex));
}

Indexer::~Indexer()
{
    mSyncer->stop();
    mSyncer->wait();
}

void Indexer::commitDependencies(const DependencyHash& deps, bool sync)
{
    DependencyHash newDependencies;

    if (mDependencies.isEmpty()) {
        mDependencies = deps;
        newDependencies = deps;
    } else {
        const DependencyHash::const_iterator end = deps.end();
        for (DependencyHash::const_iterator it = deps.begin(); it != end; ++it) {
            newDependencies[it.key()].unite(it.value() - mDependencies[it.key()]);
            DependencyHash::iterator i = newDependencies.find(it.key());
            if (i.value().isEmpty())
                newDependencies.erase(i);
            mDependencies[it.key()].unite(it.value());
        }
    }
    if (sync && !newDependencies.isEmpty())
        mSyncer->addDependencies(newDependencies);

    Path parentPath;
    QSet<QString> watchPaths;
    const DependencyHash::const_iterator end = newDependencies.end();
    QMutexLocker lock(&mWatchedMutex);
    for (DependencyHash::const_iterator it = newDependencies.begin(); it != end; ++it) {
        const Path& path = it.key();
        parentPath = path.parentDir();
        WatchedHash::iterator wit = mWatched.find(parentPath);
        //debug() << "watching" << path << "in" << parentPath;
        if (wit == mWatched.end()) {
            mWatched[parentPath].insert(qMakePair<QByteArray, quint64>(path.fileName(), path.lastModified()));
            watchPaths.insert(QString::fromLocal8Bit(parentPath));
        } else {
            wit.value().insert(qMakePair<QByteArray, quint64>(path.fileName(), path.lastModified()));
        }
    }
    if (watchPaths.isEmpty())
        return;
    mWatcher.addPaths(watchPaths.toList());
}

int Indexer::index(const QByteArray& input, const QList<QByteArray>& arguments)
{
    QMutexLocker locker(&mMutex);

    if (mIndexing.contains(input))
        return -1;

    const int id = ++mJobCounter;
    IndexerJob* job = new IndexerJob(this, id, input, arguments);
    connect(job, SIGNAL(done(int, Path, bool, QByteArray)),
            this, SLOT(onJobComplete(int, Path, bool, QByteArray)));
    if (needsToWaitForPch(job)) {
        mWaitingForPCH[id] = job;
        return id;
    }
    startJob(id, job);
    return id;
}

void Indexer::startJob(int id, IndexerJob *job)
{
    mJobs[id] = job;
    mIndexing.insert(job->mIn);

    if (!mTimerRunning) {
        mTimerRunning = true;
        mTimer.start();
    }

    QThreadPool::globalInstance()->start(job);
}

void Indexer::customEvent(QEvent* e)
{
    if (e->type() == static_cast<QEvent::Type>(DependencyEvent::Type)) {
        commitDependencies(static_cast<DependencyEvent*>(e)->deps, true);
    }
}

void Indexer::onDirectoryChanged(const QString& path)
{
    const Path p = path.toLocal8Bit();
    Q_ASSERT(p.endsWith('/'));
    QMutexLocker lock(&mWatchedMutex);
    WatchedHash::iterator it = mWatched.find(p);
    if (it == mWatched.end()) {
        error() << "directory changed, but not in watched list" << p;
        return;
    }

    Path file;
    QList<Path> pending;
    QSet<WatchedPair>::iterator wit = it.value().begin();
    QSet<WatchedPair>::const_iterator wend = it.value().end();
    QList<QByteArray> args;
    QSet<Path> dirtyFiles;
    QHash<Path, QList<QByteArray> > toIndex, toIndexPch;

    leveldb::DB *db = Server::instance()->db(Server::FileInformation);
    while (wit != wend) {
        // weird API, QSet<>::iterator does not allow for modifications to the referenced value
        file = (p + (*wit).first);
        debug() << "comparing" << file << (file.lastModified() == (*wit).second)
                << QDateTime::fromTime_t(file.lastModified());
        if (!file.exists() || file.lastModified() != (*wit).second) {
            dirtyFiles.insert(file);
            pending.append(file);
            wit = it.value().erase(wit);
            wend = it.value().end(); // ### do we need to update 'end' here?

            DependencyHash::const_iterator dit = mDependencies.find(file);
            if (dit == mDependencies.end()) {
                error() << "file modified but not in dependency list" << file;
                ++it;
                continue;
            }
            Q_ASSERT(!dit.value().isEmpty());
            foreach (const Path& path, dit.value()) {
                dirtyFiles.insert(path);
                if (path.exists()) {
                    bool ok;
                    const FileInformation fi = Rdm::readValue<FileInformation>(db, path, &ok);
                    if (ok) {
                        if (isPch(fi.compileArgs)) {
                            toIndexPch[path] = fi.compileArgs;
                        } else {
                            toIndex[path] = fi.compileArgs;
                        }
                    }
                }
            }
        } else {
            ++wit;
        }
    }

    foreach(const Path& path, pending) {
        it.value().insert(qMakePair<QByteArray, quint64>(path.fileName(), path.lastModified()));
    }
    if (toIndex.isEmpty() && toIndexPch.isEmpty())
        return;

    lock.unlock();
    QThreadPool::globalInstance()->start(new DirtyJob(this, dirtyFiles, toIndexPch, toIndex));
}

void Indexer::onJobComplete(int id, const Path& input, bool isPch, const QByteArray &msg)
{
    Q_UNUSED(input);

    QMutexLocker locker(&mMutex);
    mJobs.remove(id);
    mIndexing.remove(input);
    if (isPch) {
        QHash<int, IndexerJob*>::iterator it = mWaitingForPCH.begin();
        while (it != mWaitingForPCH.end()) {
            IndexerJob *job = it.value();
            if (!needsToWaitForPch(job)) {
                const int id = it.key();
                it = mWaitingForPCH.erase(it);
                startJob(id, job);
            } else {
                ++it;
            }
        }
    }
    const int idx = mJobCounter - (mIndexing.size() + mWaitingForPCH.size());
    error("%s %d/%d %.1f%%", msg.constData(), idx, mJobCounter, (double(idx) / double(mJobCounter)) * 100.0);

    if (mJobs.isEmpty()) {
        mSyncer->notify();

        Q_ASSERT(mTimerRunning);
        mTimerRunning = false;
        error() << "jobs took" << ((double)(mTimer.elapsed()) / 1000.0) << "secs";
        emit jobsComplete();
    }

    emit indexingDone(id);
}

void Indexer::setDefaultArgs(const QList<QByteArray> &args)
{
    mDefaultArgs = args;
}

void Indexer::setPchDependencies(const Path &pchHeader, const QSet<Path> &deps)
{
    QWriteLocker lock(&mPchDependenciesLock);
    if (deps.isEmpty()) {
        mPchDependencies.remove(pchHeader);
    } else {
        mPchDependencies[pchHeader] = deps;
    }
    mSyncer->setPchDependencies(mPchDependencies);
}

QSet<Path> Indexer::pchDependencies(const Path &pchHeader) const
{
    QReadLocker lock(&mPchDependenciesLock);
    return mPchDependencies.value(pchHeader);
}

PchUSRHash Indexer::pchUSRHash(const QList<Path> &pchFiles) const
{
    QReadLocker lock(&mPchUSRHashLock);
    const int count = pchFiles.size();
    switch (pchFiles.size()) {
    case 0: return PchUSRHash();
    case 1: return mPchUSRHashes.value(pchFiles.first());
    default:
        break;
    }
    PchUSRHash ret = mPchUSRHashes.value(pchFiles.first());
    for (int i=1; i<count; ++i) {
        const PchUSRHash h = mPchUSRHashes.value(pchFiles.at(i));
        for (PchUSRHash::const_iterator it = h.begin(); it != h.end(); ++it) {
            ret[it.key()] = it.value();
        }
    }
    return ret;
}

void Indexer::setPchUSRHash(const Path &pch, const PchUSRHash &astHash)
{
    mSyncer->addPchUSRHash(pch, astHash);
    QWriteLocker lock(&mPchUSRHashLock);
    mPchUSRHashes[pch] = astHash;
}
bool Indexer::needsToWaitForPch(IndexerJob *job) const
{
    foreach(const Path &pchHeader, job->mPchHeaders) {
        if (mIndexing.contains(pchHeader))
            return true;
    }
    return false;
}
