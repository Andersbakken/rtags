#include "Database.h"
#include "Server.h"
#include "DirtyJob.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "MemoryMonitor.h"
#include "Path.h"
#include "RTags.h"
#include "Rdm.h"
#include "SHA256.h"
#include "ReadLocker.h"
#include "RegExp.h"
#include "WriteLocker.h"
#include <Log.h>

Indexer::Indexer(const ByteArray &path, QObject *parent)
    : QObject(parent), mShuttingDown(false)
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

    connect(&mWatcher, SIGNAL(modified(Path)),
            this, SLOT(onDirectoryChanged(Path)));

    {
        ScopedDB db = Server::instance()->db(Server::PCHUsrMapes, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            mPchUSRMapes[it->key().byteArray()] = it->value<PchUSRMap>();
            it->next();
        }
    }
    {
        ScopedDB db = Server::instance()->db(Server::General, ScopedDB::Read);
        mPchDependencies = db->value<Map<Path, Set<uint32_t> > >("pchDependencies");
    }
    {
        // watcher
        ScopedDB db = Server::instance()->db(Server::Dependency, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        DependencyMap dependencies;
        while (it->isValid()) {
            const Slice key = it->key();
            const uint32_t fileId = *reinterpret_cast<const uint32_t*>(key.data());
            const Set<uint32_t> deps = it->value<Set<uint32_t> >();
            dependencies[fileId] = deps;
            it->next();
        }
        commitDependencies(dependencies, false);
    }

    initDB();
}

Indexer::~Indexer()
{
    MutexLocker locker(&mMutex);
    mShuttingDown = true;
    mCondition.wakeAll();
    // write out FileInformation for all the files that are waiting for pch maybe
}

static inline bool isFile(uint32_t fileId)
{
    return Location::path(fileId).isFile(); // ### not ideal
}

void Indexer::initDB(InitMode mode, const ByteArray &pattern)
{
    Q_ASSERT(mode == ForceDirty || pattern.isEmpty());
    Timer timer;
    Map<uint32_t, Set<uint32_t> > deps, depsReversed;

    ScopedDB dependencyDB = Server::instance()->db(Server::Dependency, ScopedDB::Read);
    RTags::Ptr<Iterator> it(dependencyDB->createIterator());
    it->seekToFirst();
    {
        Batch batch(dependencyDB);
        while (it->isValid()) {
            const Slice key = it->key();
            const uint32_t file = *reinterpret_cast<const uint32_t*>(key.data());
            if (isFile(file)) {
                const Set<uint32_t> v = it->value<Set<uint32_t> >();
                depsReversed[file] = v;
                foreach(const uint32_t p, v) {
                    deps[p].insert(file);
                }
            } else {
                batch.remove(key);
            }
            it->next();
        }
    }

    Set<uint32_t> dirtyFiles;
    Map<Path, List<ByteArray> > toIndex, toIndexPch;
    int checked = 0;

    {
        ScopedDB fileInformationDB = Server::instance()->db(Server::FileInformation, ScopedDB::Write);
        Batch batch(fileInformationDB);
        it.reset(fileInformationDB->createIterator());
        it->seekToFirst();
        MutexLocker lock(&mVisitedFilesMutex);
        RegExp rx(pattern);
        while (it->isValid()) {
            const Slice key = it->key();
            const uint32_t fileId = *reinterpret_cast<const uint32_t*>(key.data());
            const Path path = Location::path(fileId);
            if (path.isFile()) {
                const FileInformation fi = it->value<FileInformation>();
                if (!fi.compileArgs.isEmpty()) {
#ifdef QT_DEBUG
                    if (path.isHeader() && !Rdm::isPch(fi.compileArgs)) {
                        error() << path; // << fi.compileArgs << fileId;
                        Q_ASSERT(0);
                    }
#endif
                    ++checked;
                    bool dirty = false;
                    const Set<uint32_t> dependencies = deps.value(fileId);
                    Q_ASSERT(dependencies.contains(fileId));
                    foreach(uint32_t id, dependencies) {
                        if (dirtyFiles.contains(id)) {
                            dirty = true;
                        } else {
                            const Path p = Location::path(id);
                            if ((mode == ForceDirty && (pattern.isEmpty() || rx.indexIn(p) != -1))
                                || p.lastModified() > fi.lastTouched) {
                                dirtyFiles.insert(id);
                                dirtyFiles += depsReversed.value(id);
                                dirty = true;
                            }
                        }
                    }
                    if (dirty) {
                        if (Rdm::isPch(fi.compileArgs)) {
                            toIndexPch[path] = fi.compileArgs;
                        } else {
                            toIndex[path] = fi.compileArgs;
                        }
                    } else {
                        mVisitedFiles += dependencies;
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
        error() << "Checked " << checked << " files. Found " << dirtyFiles.size() << " dirty files and "
                << (toIndex.size() + toIndexPch.size()) << " sources to reindex in " << timer.elapsed() << "ms";

    if (toIndex.isEmpty() && toIndexPch.isEmpty())
        return;

    {
        MutexLocker lock(&mVisitedFilesMutex);
        mVisitedFiles -= dirtyFiles;
    }

    if (dirtyFiles.size() > 1) {
        Server::instance()->threadPool()->start(new DirtyJob(this, dirtyFiles, toIndexPch, toIndex));
    } else {
        for (Map<Path, List<ByteArray> >::const_iterator it = toIndexPch.begin(); it != toIndexPch.end(); ++it) {
            index(it->first, it->second, IndexerJob::DirtyPch|IndexerJob::NeedsDirty);
        }

        for (Map<Path, List<ByteArray> >::const_iterator it = toIndex.begin(); it != toIndex.end(); ++it) {
            index(it->first, it->second, IndexerJob::Dirty|IndexerJob::NeedsDirty);
        }
    }
}

void Indexer::commitDependencies(const DependencyMap &deps, bool sync)
{
    DependencyMap newDependencies;

    if (mDependencies.isEmpty()) {
        mDependencies = deps;
        newDependencies = deps;
    } else {
        const DependencyMap::const_iterator end = deps.end();
        for (DependencyMap::const_iterator it = deps.begin(); it != end; ++it) {
            newDependencies[it->first].unite(it->second - mDependencies[it->first]);
            DependencyMap::iterator i = newDependencies.find(it->first);
            if (i->second.isEmpty())
                newDependencies.erase(i);
            mDependencies[it->first].unite(it->second);
        }
    }
    if (sync && !newDependencies.isEmpty())
        Rdm::writeDependencies(newDependencies);

    Path parentPath;
    Set<ByteArray> watchPaths;
    const DependencyMap::const_iterator end = newDependencies.end();
    MutexLocker lock(&mWatchedMutex);
    for (DependencyMap::const_iterator it = newDependencies.begin(); it != end; ++it) {
        const Path path = Location::path(it->first);
        parentPath = path.parentDir();
        WatchedMap::iterator wit = mWatched.find(parentPath);
        //debug() << "watching" << path << "in" << parentPath;
        if (wit == mWatched.end()) {
            mWatched[parentPath].insert(std::pair<ByteArray, time_t>(path.fileName(), path.lastModified()));
            mWatcher.watch(parentPath);
        } else {
            wit->second.insert(std::pair<ByteArray, time_t>(path.fileName(), path.lastModified()));
        }
    }
}

void Indexer::onJobFinished(IndexerJob *job)
{
    if (job->isAborted()) {
        Set<uint32_t> visited;
        for (Map<uint32_t, IndexerJob::PathState>::const_iterator it = job->mPaths.begin(); it != job->mPaths.end(); ++it) {
            if (it->second == IndexerJob::Index)
                visited.insert(it->first);
        }

        MutexLocker lock(&mVisitedFilesMutex);
        mVisitedFiles -= visited;
        job->mMessage = "Aborted";
    }

    {
        MutexLocker locker(&mMutex);
        if (mJobs.size() == 1)
            printf("Removing %s from jobs\n", job->mIn.constData());
        mJobs.remove(job->mFileId);
        if (job->mIsPch) {
            Map<int, IndexerJob*>::iterator it = mWaitingForPCH.begin();
            while (it != mWaitingForPCH.end()) {
                IndexerJob *job = it->second;
                if (!needsToWaitForPch(job)) {
                    mWaitingForPCH.erase(it++);
                    startJob(job);
                } else {
                    ++it;
                }
            }
        }
        const int idx = mJobCounter - (mJobs.size() + mWaitingForPCH.size());
        error("[%3d%%] %d/%d %s. Pending jobs %d. %d mb mem.",
              static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
              job->mMessage.constData(), mJobs.size() + mWaitingForPCH.size(),
              int((MemoryMonitor::usage() / (1024 * 1024))));

        if (mJobs.isEmpty()) {
            Q_ASSERT(mTimerRunning);
            mTimerRunning = false;
            error() << "jobs took " << ((double)(mTimer.elapsed()) / 1000.0) << " secs, using "
                    << MemoryMonitor::usage() / (1024.0 * 1024.0) << " mb of memory";
            mJobCounter = 0;
            emit jobsComplete();
        }

        emit indexingDone(job->mId);
    }

    job->deleteLater();
    mCondition.wakeAll();
}


int Indexer::index(const Path &input, const List<ByteArray> &arguments, unsigned indexerJobFlags)
{
    MutexLocker locker(&mMutex);

    const uint32_t fileId = Location::insertFile(input);

    forever {
        IndexerJob *job = mJobs.value(fileId);
        if (job) {
            // printf("Aborting job %s to restart it\n", job->mIn.constData());
            job->abort();
        } else {
            break;
        }
        mCondition.wait(&mMutex);
        if (mShuttingDown)
            return -1;
    }

    const int id = ++mJobCounter;
    IndexerJob *job = new IndexerJob(this, id, indexerJobFlags, input, arguments);
    connect(job, SIGNAL(finished(IndexerJob*)), this, SLOT(onJobFinished(IndexerJob*)));

    if (needsToWaitForPch(job)) {
        mWaitingForPCH[id] = job;
        return id;
    }
    startJob(job);
    return id;
}

void Indexer::startJob(IndexerJob *job)
{
    // mMutex is always held at this point
    mJobs[job->mFileId] = job;

    if (!mTimerRunning) {
        mTimerRunning = true;
        mTimer.start();
    }

    Server::instance()->threadPool()->start(job, job->priority());
}

void Indexer::onDirectoryChanged(const Path &p)
{
    // printf("%s modified\n", p.constData());
    Set<uint32_t> dirtyFiles;
    Map<Path, List<ByteArray> > toIndex, toIndexPch;

    Q_ASSERT(p.endsWith('/'));
    {
        MutexLocker watchedLock(&mWatchedMutex);
        MutexLocker visitedLock(&mVisitedFilesMutex);
        WatchedMap::iterator it = mWatched.find(p);
        if (it == mWatched.end()) {
            error() << "directory changed, but not in watched list" << p;
            return;
        }

        Path file;
        List<Path> pending;
        Set<WatchedPair>::iterator wit = it->second.begin();
        Set<WatchedPair>::const_iterator wend = it->second.end();
        List<ByteArray> args;

        ScopedDB db = Server::instance()->db(Server::FileInformation, ScopedDB::Read);
        while (wit != wend) {
            // weird API, Set<>::iterator does not allow for modifications to the referenced value
            file = (p + (*wit).first);
            // error() << "comparing" << file << (file.lastModified() == (*wit).second)
            //          << QDateTime::fromTime_t(file.lastModified());
            if (!file.exists() || file.lastModified() != (*wit).second) {
                warning() << file << " has changed";
                const uint32_t fileId = Location::fileId(file);
                dirtyFiles.insert(fileId);
                mVisitedFiles.remove(fileId);
                pending.append(file);
                it->second.erase(wit++);
                wend = it->second.end(); // ### do we need to update 'end' here?

                DependencyMap::const_iterator dit = mDependencies.find(fileId);
                if (dit == mDependencies.end()) {
                    error() << "file modified but not in dependency list" << file;
                    ++it;
                    continue;
                }
                Q_ASSERT(!dit->second.isEmpty());
                foreach (uint32_t pathId, dit->second) {
                    dirtyFiles.insert(pathId);
                    mVisitedFiles.remove(pathId);
                    const Path path = Location::path(pathId);
                    if (path.exists()) {
                        bool ok;
                        char buf[4];
                        memcpy(buf, &pathId, 4);
                        const FileInformation fi = db->value<FileInformation>(Slice(buf, 4), &ok);
                        if (ok) {
                            if (Rdm::isPch(fi.compileArgs)) {
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

        foreach(const Path &path, pending) {
            it->second.insert(std::pair<ByteArray, time_t>(path.fileName(), path.lastModified()));
        }
    }
    if (toIndex.isEmpty() && toIndexPch.isEmpty())
        return;

    if (dirtyFiles.size() > 1) {
        Server::instance()->threadPool()->start(new DirtyJob(this, dirtyFiles, toIndexPch, toIndex));
    } else {
        for (Map<Path, List<ByteArray> >::const_iterator it = toIndexPch.begin(); it != toIndexPch.end(); ++it) {
            index(it->first, it->second, IndexerJob::DirtyPch|IndexerJob::NeedsDirty);
        }

        for (Map<Path, List<ByteArray> >::const_iterator it = toIndex.begin(); it != toIndex.end(); ++it) {
            index(it->first, it->second, IndexerJob::Dirty|IndexerJob::NeedsDirty);
        }
    }
}

void Indexer::setPchDependencies(const Path &pchHeader, const Set<uint32_t> &deps)
{
    WriteLocker lock(&mPchDependenciesLock);
    if (deps.isEmpty()) {
        mPchDependencies.remove(pchHeader);
    } else {
        mPchDependencies[pchHeader] = deps;
    }
    Rdm::writePchDepencies(mPchDependencies);
}

Set<uint32_t> Indexer::pchDependencies(const Path &pchHeader) const
{
    ReadLocker lock(&mPchDependenciesLock);
    return mPchDependencies.value(pchHeader);
}

void Indexer::addDependencies(const DependencyMap &deps)
{
    MutexLocker lock(&mMutex);
    commitDependencies(deps, true);
}

PchUSRMap Indexer::pchUSRMap(const List<Path> &pchFiles) const
{
    ReadLocker lock(&mPchUSRMapLock);
    const int count = pchFiles.size();
    switch (pchFiles.size()) {
    case 0: return PchUSRMap();
    case 1: return mPchUSRMapes.value(pchFiles.front());
    default:
        break;
    }
    PchUSRMap ret = mPchUSRMapes.value(pchFiles.front());
    for (int i=1; i<count; ++i) {
        const PchUSRMap h = mPchUSRMapes.value(pchFiles.at(i));
        for (PchUSRMap::const_iterator it = h.begin(); it != h.end(); ++it) {
            ret[it->first] = it->second;
        }
    }
    return ret;
}

void Indexer::setPchUSRMap(const Path &pch, const PchUSRMap &astMap)
{
    WriteLocker lock(&mPchUSRMapLock);
    mPchUSRMapes[pch] = astMap;
    Rdm::writePchUSRMapes(mPchUSRMapes);
}
bool Indexer::needsToWaitForPch(IndexerJob *job) const
{
    foreach(const Path &pchHeader, job->mPchHeaders) {
        const uint32_t fileId = Location::fileId(pchHeader);
        if (mJobs.contains(fileId))
            return true;
    }
    return false;
}

void Indexer::abort()
{
    MutexLocker lock(&mMutex);
    for (Map<int, IndexerJob*>::const_iterator it = mWaitingForPCH.begin(); it != mWaitingForPCH.end(); ++it) {
        delete it->second;
    }

    mWaitingForPCH.clear();
    for (Map<int, IndexerJob*>::const_iterator it = mJobs.begin(); it != mJobs.end(); ++it) {
        it->second->abort();
    }
}

ByteArray Indexer::fixIts(const Path &path) const
{
    uint32_t fileId = Location::fileId(path);
    if (!fileId)
        return ByteArray();
    ReadLocker lock(&mFixItsAndErrorsLock);
    Map<Location, std::pair<int, ByteArray> >::const_iterator it = mFixIts.lower_bound(Location(fileId, 0));
    ByteArray ret;
    char buf[1024];
    while (it != mFixIts.end() && it->first.fileId() == fileId) {
        int w;
        if ((*it).second.first) {
            w = snprintf(buf, sizeof(buf), "%d-%d %s%s", it->first.offset(), (*it).second.first,
                         (*it).second.second.constData(), ret.isEmpty() ? "" : "\n");
        } else {
            w = snprintf(buf, sizeof(buf), "%d %s%s", it->first.offset(),
                         (*it).second.second.constData(), ret.isEmpty() ? "" : "\n");
        }
        ret.prepend(ByteArray(buf, w)); // we want the last ones front()
        ++it;
    }
    return ret;
}

ByteArray Indexer::errors(const Path &path) const
{
    uint32_t fileId = Location::fileId(path);
    if (!fileId)
        return ByteArray();
    ReadLocker lock(&mFixItsAndErrorsLock);
    return mErrors.value(fileId);
}


void Indexer::setDiagnostics(const Map<uint32_t, List<ByteArray> > &diagnostics,
                             const Map<Location, std::pair<int, ByteArray> > &fixIts)
{
    WriteLocker lock(&mFixItsAndErrorsLock);

    for (Map<uint32_t, List<ByteArray> >::const_iterator it = diagnostics.begin(); it != diagnostics.end(); ++it) {
        const uint32_t fileId = it->first;
        Map<Location, std::pair<int, ByteArray> >::iterator i = mFixIts.lower_bound(Location(fileId, 0));
        while (i != mFixIts.end() && i->first.fileId() == fileId) {
            mFixIts.erase(i++);
        }
        if (it->second.isEmpty()) {
            mErrors.remove(it->first);
        } else {
            mErrors[it->first] = ByteArray::join(it->second, "\n");
        }
    }
    for (Map<Location, std::pair<int, ByteArray> >::const_iterator it = fixIts.begin(); it != fixIts.end(); ++it) {
        mFixIts[it->first] = (*it).second;
    }
}

void Indexer::reindex(const ByteArray &pattern)
{
    initDB(ForceDirty, pattern);
}
