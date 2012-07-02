#include "Database.h"
#include "Server.h"
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
#include <math.h>

Indexer::Indexer(bool validate)
{
    mJobCounter = 0;
    mTimerRunning = false;

    mWatcher.modified().connect(this, &Indexer::onDirectoryChanged);
    {
        ScopedDB db = Server::instance()->db(Server::PCHUsrMaps, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        while (it->isValid()) {
            mPchUSRMaps[it->key().byteArray()] = it->value<PchUSRMap>();
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

    initDB(validate ? Normal : NoValidate);
}

Indexer::~Indexer()
{
}

static inline bool isFile(uint32_t fileId)
{
    return Location::path(fileId).isFile();
}

void Indexer::initDB(InitMode mode, const ByteArray &pattern)
{
    assert(mode == ForceDirty || pattern.isEmpty());
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
                for (Set<uint32_t>::const_iterator vit = v.begin(); vit != v.end(); ++vit) {
                    deps[*vit].insert(file);
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
#ifdef RTAGS_DEBUG
                    if (path.isHeader() && !Rdm::isPch(fi.compileArgs)) {
                        error() << path << fi.compileArgs << fileId;
                        assert(0);
                    }
#endif
                    ++checked;
                    bool dirty = false;
                    const Set<uint32_t> dependencies = deps.value(fileId);
                    assert(dependencies.contains(fileId));
                    if (mode != NoValidate) {
                        for (Set<uint32_t>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
                            const uint32_t id = *it;
                            if (dirtyFiles.contains(id)) {
                                dirty = true;
                            } else {
                                const Path p = Location::path(id);
                                bool pdirty = false;
                                switch (mode) {
                                case NoValidate:
                                    assert(0);
                                    break;
                                case ForceDirty:
                                    if (pattern.isEmpty() || rx.indexIn(p) != -1) {
                                        pdirty = true;
                                        break;
                                    }
                                    // fall through
                                case Normal:
                                    pdirty = (p.lastModified() > fi.lastTouched);
                                    break;
                                }
                                if (pdirty) {
                                    dirty = true;
                                    dirtyFiles.insert(id);
                                    dirtyFiles += depsReversed.value(id);
                                }
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
                    warning() << "checking if " << path << " is dirty => " << dirty;
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

    for (Map<Path, List<ByteArray> >::const_iterator it = toIndexPch.begin(); it != toIndexPch.end(); ++it) {
        index(it->first, it->second, IndexerJob::DirtyPch, dirtyFiles);
    }

    for (Map<Path, List<ByteArray> >::const_iterator it = toIndex.begin(); it != toIndex.end(); ++it) {
        index(it->first, it->second, IndexerJob::Dirty, dirtyFiles);
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

void Indexer::event(const Event *event)
{
    switch (event->type()) {
    case IndexerJobFinishedEvent::Type:
        onJobFinished(static_cast<const IndexerJobFinishedEvent*>(event)->job);
        break;
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
        job->mMessage += job->mIn + " Aborted";
    }

    {
        MutexLocker locker(&mMutex);
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
        IndexerJob *waiting = 0;
        if (mWaitingForAbort.remove(job->mFileId, &waiting)) {
            assert(waiting);
            startJob(waiting);
        }

        if (mJobs.isEmpty()) {
            assert(mTimerRunning);
            mTimerRunning = false;
            error() << "jobs took " << ((double)(mTimer.elapsed()) / 1000.0) << " secs, using "
                    << MemoryMonitor::usage() / (1024.0 * 1024.0) << " mb of memory";
            mJobCounter = 0;
            jobsComplete()();
        }

        mIndexingDone(job->mId);
    }

    delete job;
}


int Indexer::index(const Path &input, const List<ByteArray> &arguments,
                   unsigned indexerJobFlags, const Set<uint32_t> &dirty)
{
    MutexLocker locker(&mMutex);

    const uint32_t fileId = Location::insertFile(input);

    const int id = ++mJobCounter;
    IndexerJob *job = new IndexerJob(this, id, indexerJobFlags, input, arguments, dirty);

    if (needsToWaitForPch(job)) {
        mWaitingForPCH[id] = job;
        return id;
    }

    IndexerJob *existing = mJobs.value(fileId);
    if (existing) {
        existing->abort();
        job->mFlags |= (existing->mFlags & (IndexerJob::Dirty|IndexerJob::DirtyPch));
        IndexerJob *&j = mWaitingForAbort[fileId];
        // ### if we're already waiting for this file, is it worth it to spawn a
        // ### new thread? what about the id?
        delete j;
        j = job;
        return -1;
    }

    startJob(job);
    return id;
}

void Indexer::startJob(IndexerJob *job)
{
    assert(!mJobs.contains(job->mFileId));
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

    assert(p.endsWith('/'));
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
            warning() << "comparing " << file << " " << (file.lastModified() == (*wit).second)
                      << " " << Rdm::timeToString(file.lastModified());
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
                assert(!dit->second.isEmpty());
                for (Set<uint32_t>::const_iterator pit = dit->second.begin(); pit != dit->second.end(); ++pit) {
                    const uint32_t pathId = *pit;
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

        const int pendingCount = pending.size();
        for (int i=0; i<pendingCount; ++i) {
            const Path &path = pending.at(i);
            it->second.insert(std::pair<ByteArray, time_t>(path.fileName(), path.lastModified()));
        }
    }

    for (Map<Path, List<ByteArray> >::const_iterator it = toIndexPch.begin(); it != toIndexPch.end(); ++it) {
        index(it->first, it->second, IndexerJob::DirtyPch, dirtyFiles);
    }

    for (Map<Path, List<ByteArray> >::const_iterator it = toIndex.begin(); it != toIndex.end(); ++it) {
        index(it->first, it->second, IndexerJob::Dirty, dirtyFiles);
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

Set<uint32_t> Indexer::dependencies(uint32_t fileId) const
{
    MutexLocker lock(&mMutex);
    return mDependencies.value(fileId);
}


PchUSRMap Indexer::pchUSRMap(const List<Path> &pchFiles) const
{
    ReadLocker lock(&mPchUSRMapLock);
    const int count = pchFiles.size();
    switch (pchFiles.size()) {
    case 0: return PchUSRMap();
    case 1: return mPchUSRMaps.value(pchFiles.front());
    default:
        break;
    }
    PchUSRMap ret = mPchUSRMaps.value(pchFiles.front());
    for (int i=1; i<count; ++i) {
        const PchUSRMap h = mPchUSRMaps.value(pchFiles.at(i));
        for (PchUSRMap::const_iterator it = h.begin(); it != h.end(); ++it) {
            ret[it->first] = it->second;
        }
    }
    return ret;
}

void Indexer::setPchUSRMap(const Path &pch, const PchUSRMap &astMap)
{
    WriteLocker lock(&mPchUSRMapLock);
    mPchUSRMaps[pch] = astMap;
    Rdm::writePchUSRMaps(mPchUSRMaps);
}
bool Indexer::needsToWaitForPch(IndexerJob *job) const
{
    for (List<Path>::const_iterator it = job->mPchHeaders.begin(); it != job->mPchHeaders.end(); ++it) {
        const Path &pchHeader = *it;
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
