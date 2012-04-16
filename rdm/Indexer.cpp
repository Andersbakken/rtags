#include "Database.h"
#include "DependencyEvent.h"
#include "DirtyJob.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "IndexerSyncer.h"
#include "LevelDB.h"
#include "Path.h"
#include "RTags.h"
#include "Rdm.h"
#include "SHA256.h"
#include "leveldb/write_batch.h"
#include <Log.h>
#include <QtCore>

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

Indexer::Indexer(const QByteArray& path, QObject* parent)
    : QObject(parent)
{
    Q_ASSERT(path.startsWith('/'));
    if (!path.startsWith('/'))
        return;
    QDir dir;
    dir.mkpath(path);

    mJobCounter = 0;
    mLastJobId = 0;
    mPath = path;
    if (!mPath.endsWith('/'))
        mPath += '/';
    mTimerRunning = false;
    mSyncer = new IndexerSyncer(this);
    mSyncer->start();

    connect(&mWatcher, SIGNAL(directoryChanged(QString)),
            this, SLOT(onDirectoryChanged(QString)));

    initWatcher();
    init();
}

Indexer::~Indexer()
{
    mSyncer->stop();
    mSyncer->wait();
}

int Indexer::index(const QByteArray& input, const QList<QByteArray>& arguments)
{
    QMutexLocker locker(&mMutex);

    if (mIndexing.contains(input))
        return -1;

    int id;
    do {
        id = mLastJobId++;
    } while (mJobs.contains(id));

    mIndexing.insert(input);

    IndexerJob* job = new IndexerJob(this, id, mPath, input, arguments);
    mJobs[id] = job;
    connect(job, SIGNAL(done(int, QByteArray)), this, SLOT(onJobDone(int, QByteArray)), Qt::QueuedConnection);

    if (!mTimerRunning) {
        mTimerRunning = true;
        mTimer.start();
    }

    QThreadPool::globalInstance()->start(job);

    return id;
}

void Indexer::customEvent(QEvent* e)
{
    if (e->type() == static_cast<QEvent::Type>(DependencyEvent::Type)) {
        commitDependencies(static_cast<DependencyEvent*>(e)->deps, true);
    }
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

    LevelDB db;
    QByteArray err;
    if (!db.open(Database::FileInformation, LevelDB::ReadOnly, &err)) {
        // ### there is a gap here where if the syncer thread hasn't synced the file information
        //     then fileInformation() would return 'false' even though it knows what args to return.
        error("Can't open FileInformation database %s %s\n",
              Database::databaseName(Database::FileInformation).constData(),
              err.constData());
        return;
    }
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
                    const FileInformation fi = Rdm::readValue<FileInformation>(db.db(), path, &ok);
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

void Indexer::onJobDone(int id, const QByteArray& input)
{
    Q_UNUSED(input);

    QMutexLocker locker(&mMutex);
    mJobs.remove(id);
    if (mIndexing.remove(input))
        mCondition.wakeAll();

    ++mJobCounter;

    if (mJobs.isEmpty()) {
        mSyncer->notify();

        Q_ASSERT(mTimerRunning);
        mTimerRunning = false;
        log(0) << "jobs took" << mTimer.elapsed() << "ms";
    }

    emit indexingDone(id);
}

void Indexer::setDefaultArgs(const QList<QByteArray> &args)
{
    mDefaultArgs = args;
}


void Indexer::initWatcher()
{
    LevelDB db;
    if (!db.open(Database::Dependency, LevelDB::ReadOnly))
        return;

    leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
    it->SeekToFirst();
    DependencyHash dependencies;
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        const Path file(key.data(), key.size());
        if (file != "pch") {
            const QSet<Path> deps = Rdm::readValue<QSet<Path> >(it);
            dependencies[file] = deps;
        } else {
            mPchDependencies = Rdm::readValue<DependencyHash>(it);
        }
        it->Next();
    }
    commitDependencies(dependencies, false);
    delete it;
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

void Indexer::timerEvent(QTimerEvent *e)
{
    if (e->timerId() == mPollTimer.timerId()) {
        poll();
    } else {
        QObject::timerEvent(e);
    }
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

void Indexer::init()
{
    DependencyHash deps;
    LevelDB fileInformationDB, dependencyDB;
    if (!fileInformationDB.open(Database::FileInformation, LevelDB::ReadOnly)
        || !dependencyDB.open(Database::Dependency, LevelDB::ReadOnly)) {
        return;
    }
    leveldb::Iterator* it = dependencyDB.db()->NewIterator(leveldb::ReadOptions());
    it->SeekToFirst();
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        const Path file(key.data(), key.size());
        if (file != "pch") {
            foreach(const Path &p, Rdm::readValue<QSet<Path> >(it)) {
                deps[p].insert(file);
            }
        }
        it->Next();
    }
    delete it;
    QSet<Path> dirty;
    QHash<Path, QList<QByteArray> > toIndex, toIndexPch;

    it = fileInformationDB.db()->NewIterator(leveldb::ReadOptions());
    it->SeekToFirst();
    while (it->Valid()) {
        const leveldb::Slice key = it->key();
        const Path path(key.data(), key.size());
        const FileInformation fi = Rdm::readValue<FileInformation>(it);
        if (isDirty(path, deps.value(path), fi.lastTouched, dirty)) {
            // ### am I checking pch deps correctly here?
            if (isPch(fi.compileArgs)) {
                toIndexPch[path] = fi.compileArgs;
            } else {
                toIndex[path] = fi.compileArgs;
            }
        }
        it->Next();
    }
    delete it;
    if (toIndex.isEmpty() && toIndexPch.isEmpty())
        return;
    
    QThreadPool::globalInstance()->start(new DirtyJob(this, dirty, toIndexPch, toIndex));
   
}

void Indexer::poll()
{

}
