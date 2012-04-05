#include "Database.h"
#include "DependencyEvent.h"
#include "DirtyJob.h"
#include "Indexer.h"
#include "IndexerImpl.h"
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

#define SYNCINTERVAL 10

inline void IndexerImpl::commitDependencies(const DependencyHash& deps, bool sync)
{
    DependencyHash newDependencies;

    if (dependencies.isEmpty()) {
        dependencies = deps;
        newDependencies = deps;
    } else {
        const DependencyHash::const_iterator end = deps.end();
        for (DependencyHash::const_iterator it = deps.begin(); it != end; ++it) {
            newDependencies[it.key()].unite(it.value() - dependencies[it.key()]);
            DependencyHash::iterator i = newDependencies.find(it.key());
            if (i.value().isEmpty())
                newDependencies.erase(i);
            dependencies[it.key()].unite(it.value());
        }
    }
    if (sync && !newDependencies.isEmpty())
        syncer->addDependencies(newDependencies);


    Path parentPath;
    QSet<QString> watchPaths;
    const DependencyHash::const_iterator end = newDependencies.end();
    QMutexLocker lock(&watchedMutex);
    for (DependencyHash::const_iterator it = newDependencies.begin(); it != end; ++it) {
        const Path& path = it.key();
        parentPath = path.parentDir();
        WatchedHash::iterator it = watched.find(parentPath);
        //debug() << "watching" << path << "in" << parentPath;
        if (it == watched.end()) {
            watched[parentPath].insert(qMakePair<QByteArray, quint64>(path.fileName(), path.lastModified()));
            watchPaths.insert(QString::fromLocal8Bit(parentPath));
        } else {
            it.value().insert(qMakePair<QByteArray, quint64>(path.fileName(), path.lastModified()));
        }
    }
    if (watchPaths.isEmpty())
        return;
    watcher.addPaths(watchPaths.toList());
}

void IndexerImpl::setPchDependencies(const Path &pchHeader, const QSet<Path> &deps)
{
    QWriteLocker lock(&pchDependenciesLock);
    if (deps.isEmpty()) {
        pchDeps.remove(pchHeader);
    } else {
        pchDeps[pchHeader] = deps;
    }
    syncer->setPchDependencies(pchDeps);
}

QSet<Path> IndexerImpl::pchDependencies(const Path &pchHeader) const
{
    QReadLocker lock(&pchDependenciesLock);
    return pchDeps.value(pchHeader);
}

Indexer* Indexer::sInst = 0;

Indexer::Indexer(const QByteArray& path, QObject* parent)
    : QObject(parent), mImpl(new IndexerImpl)
{
    Q_ASSERT(path.startsWith('/'));
    if (!path.startsWith('/'))
        return;
    QDir dir;
    dir.mkpath(path);

    mImpl->indexer = this;
    mImpl->jobCounter = 0;
    mImpl->lastJobId = 0;
    mImpl->path = path;
    if (!mImpl->path.endsWith('/'))
        mImpl->path += '/';
    mImpl->timerRunning = false;
    mImpl->syncer = new IndexerSyncer(this);
    mImpl->syncer->start();

    connect(&mImpl->watcher, SIGNAL(directoryChanged(QString)),
            this, SLOT(onDirectoryChanged(QString)));

    initWatcher();
    sInst = this;
}

Indexer::~Indexer()
{
    sInst = 0;
    mImpl->syncer->stop();
    mImpl->syncer->wait();

    delete mImpl;
}

Indexer* Indexer::instance()
{
    return sInst;
}

int Indexer::index(const QByteArray& input, const QList<QByteArray>& arguments)
{
    QMutexLocker locker(&mImpl->implMutex);

    if (mImpl->indexing.contains(input))
        return -1;

    int id;
    do {
        id = mImpl->lastJobId++;
    } while (mImpl->jobs.contains(id));

    mImpl->indexing.insert(input);

    IndexerJob* job = new IndexerJob(mImpl, id, mImpl->path, input, arguments);
    mImpl->jobs[id] = job;
    connect(job, SIGNAL(done(int, QByteArray)), this, SLOT(onJobDone(int, QByteArray)), Qt::QueuedConnection);

    if (!mImpl->timerRunning) {
        mImpl->timerRunning = true;
        mImpl->timer.start();
    }

    QThreadPool::globalInstance()->start(job);

    return id;
}

void Indexer::customEvent(QEvent* e)
{
    if (e->type() == static_cast<QEvent::Type>(DependencyEvent::Type)) {
        mImpl->commitDependencies(static_cast<DependencyEvent*>(e)->deps, true);
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
        } else if (arg == "-x") { // ### this is not entirely safe, -xc++-header is allowed
            nextIsX = true;
        }
    }
    return false;
}

void Indexer::onDirectoryChanged(const QString& path)
{
    const Path p = path.toLocal8Bit();
    Q_ASSERT(p.endsWith('/'));
    QMutexLocker lock(&mImpl->watchedMutex);
    WatchedHash::iterator it = mImpl->watched.find(p);
    if (it == mImpl->watched.end()) {
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
        if (!file.exists() || file.lastModified() != (*wit).second) {
            dirtyFiles.insert(file);
            pending.append(file);
            wit = it.value().erase(wit);
            wend = it.value().end(); // ### do we need to update 'end' here?

            DependencyHash::const_iterator dit = mImpl->dependencies.find(file);
            if (dit == mImpl->dependencies.end()) {
                error() << "file modified but not in dependency list" << file;
                ++it;
                continue;
            }
            Q_ASSERT(!dit.value().isEmpty());
            foreach (const Path& path, dit.value()) {
                dirtyFiles.insert(path);
                if (path.exists()) {
                    bool ok;
                    args = Rdm::readValue<QList<QByteArray> >(db.db(), path, &ok);

                    if (ok) {
                        if (isPch(args)) {
                            toIndexPch[path] = args;
                        } else {
                            toIndex[path] = args;
                        }
                    }
                }
            }
        } else {
            ++wit;
        }
    }

    foreach (const Path& path, pending) {
        it.value().insert(qMakePair<QByteArray, quint64>(path.fileName(), path.lastModified()));
    }
    lock.unlock();
    QThreadPool::globalInstance()->start(new DirtyJob(dirtyFiles, toIndexPch, toIndex));
}

void Indexer::onJobDone(int id, const QByteArray& input)
{
    Q_UNUSED(input)

        QMutexLocker locker(&mImpl->implMutex);
    mImpl->jobs.remove(id);
    if (mImpl->indexing.remove(input))
        mImpl->implCond.wakeAll();

    ++mImpl->jobCounter;

    if (mImpl->jobs.isEmpty() || mImpl->jobCounter == SYNCINTERVAL) {
        if (mImpl->jobs.isEmpty()) {
            mImpl->syncer->notify();

            Q_ASSERT(mImpl->timerRunning);
            mImpl->timerRunning = false;
            log(0) << "jobs took" << mImpl->timer.elapsed() << "ms";
        }
    }

    emit indexingDone(id);
}

void Indexer::setDefaultArgs(const QList<QByteArray> &args)
{
    mImpl->defaultArgs = args;
}

void DirtyJob::dirty()
{
    // ### we should probably have a thread or something that stats each file we have in the db and calls dirty if the file is gone
    const leveldb::WriteOptions writeOptions;
    debug() << "DirtyJob::dirty" << mDirty;
    {
        LevelDB db;
        QByteArray err;
        if (!db.open(Database::Symbol, LevelDB::ReadWrite, &err)) {
            error("Can't open symbol database %s %s\n",
                  Database::databaseName(Database::Symbol).constData(),
                  err.constData());
        }
        leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
        leveldb::WriteBatch batch;
        bool writeBatch = false;
        it->SeekToFirst();
        while (it->Valid()) {
            const leveldb::Slice key = it->key();
            debug() << "looking at" << key.data();
            const int comma = QByteArray::fromRawData(key.data(), key.size()).lastIndexOf(',');
            Q_ASSERT(comma != -1);
            const Path p = QByteArray::fromRawData(key.data(), comma);
            if (mDirty.contains(p)) {
                debug() << "key is dirty. removing" << key.data();
                batch.Delete(key);
                writeBatch = true;
            } else {
                Rdm::CursorInfo cursorInfo = Rdm::readValue<Rdm::CursorInfo>(it);
                if (cursorInfo.dirty(mDirty)) {
                    writeBatch = true;
                    if (cursorInfo.target.isNull() && cursorInfo.references.isEmpty()) {
                        debug() << "CursorInfo is empty now. removing" << key.data();
                        batch.Delete(key);
                    } else {
                        debug() << "CursorInfo is modified. Changing" << key.data();
                        Rdm::writeValue<Rdm::CursorInfo>(&batch, key.data(), cursorInfo);
                    }
                }
            }
            it->Next();
        }
        delete it;
        if (writeBatch) {
            db.db()->Write(writeOptions, &batch);
        }
    }

    {
        LevelDB db;
        QByteArray err;
        if (!db.open(Database::SymbolName, LevelDB::ReadWrite, &err)) {
            error("Can't open symbol name database %s %s\n",
                  Database::databaseName(Database::SymbolName).constData(),
                  err.constData());
        }
        leveldb::Iterator* it = db.db()->NewIterator(leveldb::ReadOptions());
        leveldb::WriteBatch batch;
        bool writeBatch = false;
        it->SeekToFirst();
        while (it->Valid()) {
            QSet<RTags::Location> locations = Rdm::readValue<QSet<RTags::Location> >(it);
            QSet<RTags::Location>::iterator i = locations.begin();
            bool changed = false;
            while (i != locations.end()) {
                if (mDirty.contains((*i).path)) {
                    changed = true;
                    i = locations.erase(i);
                } else {
                    ++i;
                }
            }
            if (changed) {
                writeBatch = true;
                if (locations.isEmpty()) {
                    debug() << "No references to" << it->key().data() << "anymore. Removing";
                    batch.Delete(it->key());
                } else {
                    debug() << "References to" << it->key().data() << "modified. Changing";
                    Rdm::writeValue<QSet<RTags::Location> >(&batch, it->key().data(), locations);
                }
            }
            it->Next();
        }
        delete it;
        if (writeBatch) {
            db.db()->Write(writeOptions, &batch);
        }
    }
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
            mImpl->pchDeps = Rdm::readValue<DependencyHash>(it);
        }
        it->Next();
    }
    mImpl->commitDependencies(dependencies, false);

    delete it;


}
