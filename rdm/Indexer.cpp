#include "Server.h"
#include "DirtyJob.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "MemoryMonitor.h"
#include "Path.h"
#include "RTags.h"
#include "Rdm.h"
#include "SHA256.h"
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

    connect(&mWatcher, SIGNAL(directoryChanged(QString)),
            this, SLOT(onDirectoryChanged(QString)));

    {
        // fileids
        ScopedDB db = Server::instance()->db(Server::FileIds, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        QHash<quint32, Path> idsToPaths;
        QHash<Path, quint32> pathsToIds;
        while (it->isValid()) {
            const Slice key = it->key();
            const Path path(key.data(), key.size());
            const quint32 fileId = it->value<quint32>();
            idsToPaths[fileId] = path;
            pathsToIds[path] = fileId;
            it->next();
        }
        Location::init(pathsToIds, idsToPaths);
    }
    {
        ScopedDB db = Server::instance()->db(Server::PCHUsrHashes, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            mPchUSRHashes[it->key().data()] = it->value<PchUSRHash>();
            it->next();
        }
    }
    {
        ScopedDB db = Server::instance()->db(Server::General, ScopedDB::Read);
        mPchDependencies = db->value<QHash<Path, QSet<quint32> > >("pchDependencies");
    }
    {
        // watcher
        ScopedDB db = Server::instance()->db(Server::Dependency, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        DependencyHash dependencies;
        while (it->isValid()) {
            const Slice key = it->key();
            const quint32 fileId = *reinterpret_cast<const quint32*>(key.data());
            const QSet<quint32> deps = it->value<QSet<quint32> >();
            dependencies[fileId] = deps;
            it->next();
        }
        commitDependencies(dependencies, false);
    }

    initDB();
}

Indexer::~Indexer()
{
    // QMutexLocker locker(&mMutex);

    // write out FileInformation for all the files that are waiting for pch maybe
}


static inline bool isDirty(const QSet<quint32> &dependencies, quint64 time, QSet<quint32> &dirty)
{
    bool ret = false;

    for (QSet<quint32>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
        const quint32 id = *it;
        if (dirty.contains(id)) {
            ret = true;
        } else if (Location::path(id).lastModified() > time) {
            dirty.insert(id);
            ret = true;
        }
    }
    // verboseDebug() << "isDirty" << path << ret << path << QDateTime::fromTime_t(time) << dirty;
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

static inline bool isFile(quint32 fileId)
{
    return Location::path(fileId).isFile(); // ### not ideal
}

void Indexer::initDB()
{
    QElapsedTimer timer;
    timer.start();
    QHash<quint32, QSet<quint32> > deps;
    ScopedDB dependencyDB = Server::instance()->db(Server::Dependency, ScopedDB::Read);
    RTags::Ptr<Iterator> it(dependencyDB->createIterator());
    it->seekToFirst();
    {
        Batch batch(dependencyDB);
        while (it->isValid()) {
            const Slice key = it->key();
            const quint32 file = *reinterpret_cast<const quint32*>(key.data());
            if (isFile(file)) {
                foreach(quint32 p, it->value<QSet<quint32> >()) {
                    deps[p].insert(file);
                }
            } else {
                batch.remove(key);
            }
            it->next();
        }
    }

    QSet<quint32> dirtyFiles;
    QHash<Path, QList<QByteArray> > toIndex, toIndexPch;
    int checked = 0;

    {
        ScopedDB fileInformationDB = Server::instance()->db(Server::FileInformation, ScopedDB::Write);
        Batch batch(fileInformationDB);
        it.reset(fileInformationDB->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            const Slice key = it->key();
            const quint32 fileId = *reinterpret_cast<const quint32*>(key.data());
            const Path path = Location::path(fileId);
            if (path.isFile()) {
                const FileInformation fi = it->value<FileInformation>();
                if (!fi.compileArgs.isEmpty()) {
                    ++checked;
                    bool dirty = false;
                    if (isDirty(deps.value(fileId), fi.lastTouched, dirtyFiles)) {
                        dirty = true;
                        // ### am I checking pch deps correctly here?
                        if (isPch(fi.compileArgs)) {
                            toIndexPch[path] = fi.compileArgs;
                        } else {
                            toIndex[path] = fi.compileArgs;
                        }
                    }
                    warning() << "checking if" << path << "is dirty =>" << dirty;
                }
            } else {
                batch.remove(key);
            }
            it->next();
        }
    }

    if (checked)
        error() << "Checked" << checked << "files. Found" << (toIndex.size() + toIndexPch.size()) << "dirty ones in" << timer.elapsed() << "ms";

    if (toIndex.isEmpty() && toIndexPch.isEmpty())
        return;

    dirty(dirtyFiles);
    QThreadPool::globalInstance()->start(new DirtyJob(this, dirtyFiles, toIndexPch, toIndex));
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
        Rdm::writeDependencies(newDependencies);

    Path parentPath;
    QSet<QString> watchPaths;
    const DependencyHash::const_iterator end = newDependencies.end();
    QMutexLocker lock(&mWatchedMutex);
    for (DependencyHash::const_iterator it = newDependencies.begin(); it != end; ++it) {
        const Path path = Location::path(it.key());
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
    QSet<quint32> dirtyFiles;
    QHash<Path, QList<QByteArray> > toIndex, toIndexPch;

    ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Read);
    while (wit != wend) {
        // weird API, QSet<>::iterator does not allow for modifications to the referenced value
        file = (p + (*wit).first);
        // qDebug() << "comparing" << file << (file.lastModified() == (*wit).second)
        //          << QDateTime::fromTime_t(file.lastModified());
        if (!file.exists() || file.lastModified() != (*wit).second) {
            const quint32 fileId = Location::fileId(file);
            dirtyFiles.insert(fileId);
            pending.append(file);
            wit = it.value().erase(wit);
            wend = it.value().end(); // ### do we need to update 'end' here?

            DependencyHash::const_iterator dit = mDependencies.find(fileId);
            if (dit == mDependencies.end()) {
                error() << "file modified but not in dependency list" << file;
                ++it;
                continue;
            }
            Q_ASSERT(!dit.value().isEmpty());
            foreach (quint32 pathId, dit.value()) {
                dirtyFiles.insert(pathId);
                const Path path = Location::path(pathId);
                if (path.exists()) {
                    bool ok;
                    char buf[4];
                    memcpy(buf, &pathId, 4);
                    const FileInformation fi = db->value<FileInformation>(Slice(buf, 4), &ok);
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
    dirty(dirtyFiles);
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
    error("%s %d/%d %.1f%%. Pending jobs %d.",
          msg.constData(), idx, mJobCounter, (double(idx) / double(mJobCounter)) * 100.0,
          mJobs.size() + mWaitingForPCH.size());

    if (mJobs.isEmpty()) {
        Q_ASSERT(mTimerRunning);
        mTimerRunning = false;
        error() << "jobs took" << ((double)(mTimer.elapsed()) / 1000.0) << "secs"
                << "using" << (double(MemoryMonitor::usage()) / (1024.0 * 1024.0)) << "mb of memory";
        emit jobsComplete();
    }

    emit indexingDone(id);
    if (qobject_cast<IndexerJob*>(sender())->mWroteSymbolNames)
        mSymbolNamesChangedTimer.start(1000, this);
    sender()->deleteLater();
}

void Indexer::setDefaultArgs(const QList<QByteArray> &args)
{
    mDefaultArgs = args;
}

void Indexer::setPchDependencies(const Path &pchHeader, const QSet<quint32> &deps)
{
    QWriteLocker lock(&mPchDependenciesLock);
    if (deps.isEmpty()) {
        mPchDependencies.remove(pchHeader);
    } else {
        mPchDependencies[pchHeader] = deps;
    }
    Rdm::writePchDepencies(mPchDependencies);
}

QSet<quint32> Indexer::pchDependencies(const Path &pchHeader) const
{
    QReadLocker lock(&mPchDependenciesLock);
    return mPchDependencies.value(pchHeader);
}

void Indexer::addDependencies(const DependencyHash &deps)
{
    QMutexLocker lock(&mMutex);
    commitDependencies(deps, true);
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
    QWriteLocker lock(&mPchUSRHashLock);
    mPchUSRHashes[pch] = astHash;
    Rdm::writePchUSRHashes(mPchUSRHashes);
}
bool Indexer::needsToWaitForPch(IndexerJob *job) const
{
    foreach(const Path &pchHeader, job->mPchHeaders) {
        if (mIndexing.contains(pchHeader))
            return true;
    }
    return false;
}

void Indexer::abort()
{
    QMutexLocker lock(&mMutex);
    qDeleteAll(mWaitingForPCH);
    mWaitingForPCH.clear();
    foreach(IndexerJob *job, mJobs) {
        job->abort();
    }
}
QList<QByteArray> Indexer::compileArgs(const Path &file) const
{
    ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Read);
    return db->value<FileInformation>(file).compileArgs;
}
void Indexer::timerEvent(QTimerEvent *e)
{
    if (e->timerId() == mSymbolNamesChangedTimer.timerId()) {
        mSymbolNamesChangedTimer.stop();
        emit symbolNamesChanged();
    }
}

bool Indexer::visitFile(quint32 fileId)
{
    QMutexLocker lock(&mVisitedFilesMutex);
    if (mVisitedFiles.contains(fileId)) {
        return false;
    }
    mVisitedFiles.insert(fileId);
    return true;
}

void Indexer::dirty(const QSet<quint32> &files)
{
    QMutexLocker lock(&mVisitedFilesMutex);
    mVisitedFiles -= files;
}
