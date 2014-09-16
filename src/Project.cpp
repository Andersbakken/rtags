/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "Project.h"
#include "FileManager.h"
#include "DataFile.h"
#include "IndexerJob.h"
#include "RTags.h"
#include "Server.h"
#include "Server.h"
#include "JobScheduler.h"
#include "IndexData.h"
#include <math.h>
#include <fnmatch.h>
#include <rct/Log.h>
#include <rct/MemoryMonitor.h>
#include <rct/Path.h>
#include <rct/Rct.h>
#include <rct/ReadLocker.h>
#include <rct/RegExp.h>
#include <rct/Thread.h>
#include <memory>

enum {
    SyncTimeout = 500,
    DirtyTimeout = 100
};

class RestoreThread : public Thread
{
public:
    RestoreThread(const std::shared_ptr<Project> &project)
        : mPath(project->path()), mWeak(project)
    {
        setAutoDelete(true);
    }

    virtual void run()
    {
        StopWatch timer;
        RestoreThread *thread = restore() ? this : 0;
        std::mutex mutex;
        std::condition_variable condition;
        bool finished = false;

        EventLoop::mainEventLoop()->callLater([this, thread, &timer, &mutex, &condition, &finished]() {
                if (std::shared_ptr<Project> proj = mWeak.lock()) {
                    proj->updateContents(thread);
                    if (thread)
                        error() << "Restored project" << mPath << "in" << timer.elapsed() << "ms";
                }
                std::unique_lock<std::mutex> lock(mutex);
                finished = true;
                condition.notify_one();
            });

        std::unique_lock<std::mutex> lock(mutex);
        while (!finished) {
            condition.wait(lock);
        }
    }

    bool restore()
    {
        Path path = mPath;
        RTags::encodePath(path);
        const Path p = Server::instance()->options().dataDir + path;
        DataFile file(p);
        if (!file.open(DataFile::Read)) {
            if (!file.error().isEmpty())
                error("Restore error %s: %s", mPath.constData(), file.error().constData());
            Path::rm(p);
            return false;
        }

        CursorInfo::deserialize(file, mSymbols);
        file >> mSymbolNames >> mUsr >> mDependencies >> mSources >> mVisitedFiles;
        for (const auto &source : mSources) {
            mDependencies[source.second.fileId].insert(source.second.fileId);
            // if we save before finishing a sync we may have saved mSources
            // without ever having parsed them, if so they won't depend on
            // anything. Make sure they depend on themselves
        }
        return true;
    }

    const Path mPath;
    SymbolMap mSymbols;
    SymbolNameMap mSymbolNames;
    UsrMap mUsr;
    DependencyMap mDependencies;
    SourceMap mSources;
    Hash<uint32_t, Path> mVisitedFiles;
    std::weak_ptr<Project> mWeak;
};

class SyncThread : public Thread
{
public:
    SyncThread(const std::shared_ptr<Project> &project)
        : mProject(project)
    {
        setAutoDelete(true);
    }

    virtual void run()
    {
        if (std::shared_ptr<Project> project = mProject.lock()) {
            const String msg = project->sync();
            EventLoop::mainEventLoop()->callLater([project, msg]() {
                    if (!msg.isEmpty())
                        error() << msg;
                    project->onSynced();
                });
        }
    }

    std::weak_ptr<Project> mProject;
};

class Dirty
{
public:
    virtual ~Dirty() {}
    virtual Set<uint32_t> dirtied() const = 0;
    virtual bool isDirty(const Source &source) = 0;
};

class SimpleDirty : public Dirty
{
public:
    void init(const Set<uint32_t> &dirty, const DependencyMap &dependencies)
    {
        for (auto fileId : dirty) {
            mDirty.insert(fileId);
            mDirty += dependencies.value(fileId);
        }
    }

    virtual Set<uint32_t> dirtied() const
    {
        return mDirty;
    }

    virtual bool isDirty(const Source &source)
    {
        return mDirty.contains(source.fileId);
    }

    Set<uint32_t> mDirty;
};

class ComplexDirty : public Dirty
{
public:
    virtual Set<uint32_t> dirtied() const
    {
        return mDirty;
    }
    void insertDirtyFile(uint32_t fileId)
    {
        mDirty.insert(fileId);
    }
    inline uint64_t lastModified(uint32_t fileId)
    {
        uint64_t &time = mLastModified[fileId];
        if (!time) {
            time = Location::path(fileId).lastModifiedMs();
        }
        return time;
    }

    Hash<uint32_t, uint64_t> mLastModified;
    Set<uint32_t> mDirty;
};

class SuspendedDirty : public ComplexDirty
{
public:
    bool isDirty(const Source &)
    {
        return false;
    }
};

class IfModifiedDirty : public ComplexDirty
{
public:
    IfModifiedDirty(const DependencyMap &dependencies, const Match &match = Match())
        : mDependencies(dependencies), mMatch(match)
    {
        for (auto it : mDependencies) {
            const uint32_t dependee = it.first;
            const Set<uint32_t> &dependents = it.second;
            for (auto dependent : dependents) {
                mReversedDependencies[dependent].insert(dependee);
            }
        }
        // mReversedDependencies are in the form of:
        //   Path.cpp: Path.h, String.h ...
        // mDependencies are like this:
        //   Path.h: Path.cpp, Server.cpp ...
    }

    virtual bool isDirty(const Source &source)
    {
        bool ret = false;

        if (mMatch.isEmpty() || mMatch.match(source.sourceFile())) {
            for (auto it : mReversedDependencies[source.fileId]) {
                const uint64_t depLastModified = lastModified(it);
                if (!depLastModified || depLastModified > source.parsed) {
                    // dependency is gone
                    ret = true;
                    insertDirtyFile(it);
                }
            }
            if (ret)
                mDirty.insert(source.fileId);

            assert(!ret || mDirty.contains(source.fileId));
        }
        return ret;
    }

    DependencyMap mDependencies, mReversedDependencies;
    Match mMatch;
};


class WatcherDirty : public ComplexDirty
{
public:
    WatcherDirty(const DependencyMap &dependencies, const Set<uint32_t> &modified)
    {
        for (auto it : modified) {
            mModified[it] = dependencies.value(it);
        }
    }

    virtual bool isDirty(const Source &source)
    {
        bool ret = false;

        for (auto it : mModified) {
            const auto &deps = it.second;
            if (deps.contains(source.fileId)) {
                const uint64_t depLastModified = lastModified(it.first);
                if (!depLastModified || depLastModified > source.parsed) {
                    // dependency is gone
                    ret = true;
                    insertDirtyFile(it.first);
                }
            }
        }

        if (ret)
            insertDirtyFile(source.fileId);
        return ret;
    }

    DependencyMap mModified;
};

Project::Project(const Path &path)
    : mPath(path), mState(Unloaded), mJobCounter(0)
{
    const auto &options = Server::instance()->options();

    if (!(options.options & Server::NoFileSystemWatch)) {
        mWatcher.modified().connect(std::bind(&Project::onFileModifiedOrRemoved, this, std::placeholders::_1));
        mWatcher.removed().connect(std::bind(&Project::onFileModifiedOrRemoved, this, std::placeholders::_1));
    }
    if (!(options.options & Server::NoFileManagerWatch)) {
        mWatcher.removed().connect(std::bind(&Project::reloadFileManager, this));
        mWatcher.added().connect(std::bind(&Project::reloadFileManager, this));
    }
    mSyncTimer.timeout().connect([this](Timer *) { this->startSync(Sync_Asynchronous); });
    mDirtyTimer.timeout().connect(std::bind(&Project::onDirtyTimeout, this, std::placeholders::_1));
}

Project::~Project()
{
    assert(EventLoop::isMainThread());
    for (const auto &job : mActiveJobs) {
        assert(job.second);
        Server::instance()->jobScheduler()->abort(job.second);
    }
}

void Project::init()
{
    assert(mState == Unloaded);
    mState = Inited;
    fileManager.reset(new FileManager);
    fileManager->init(shared_from_this(), FileManager::Asynchronous);
}

void Project::updateContents(RestoreThread *thread)
{
    assert(EventLoop::isMainThread());
    if (state() != Loading)
        return;

    bool needsSave = false;
    std::unique_ptr<ComplexDirty> dirty;
    if (thread) {
        mSymbols = std::move(thread->mSymbols);
        mSymbolNames = std::move(thread->mSymbolNames);
        mUsr = std::move(thread->mUsr);;
        mDependencies = std::move(thread->mDependencies);
        mSources = std::move(thread->mSources);

        for (const auto& dep : mDependencies) {
            watch(Location::path(dep.first));
        }

        mVisitedFiles = std::move(thread->mVisitedFiles);
        if (Server::instance()->suspended()) {
            dirty.reset(new SuspendedDirty);
        } else {
            dirty.reset(new IfModifiedDirty(mDependencies));
        }

        {
            auto it = mDependencies.begin();

            while (it != mDependencies.end()) {
                const Path path = Location::path(it->first);
                if (!path.isFile()) {
                    error() << path << "seems to have disappeared";
                    dirty.get()->insertDirtyFile(it->first);

                    const Set<uint32_t> &dependents = it->second;
                    for (auto dependent : dependents) {
                        // we don't have a file to compare with to
                        // know whether the source is parsed after the
                        // file was removed... so, force sources
                        // dirty.
                        dirty.get()->insertDirtyFile(dependent);
                    }

                    mDependencies.erase(it++);
                    needsSave = true;
                }
                else {
                    ++it;
                }
            }
        }

        auto it = mSources.begin();
        while (it != mSources.end()) {
            const Source &source = it->second;
            if (!source.sourceFile().isFile()) {
                error() << source.sourceFile() << "seems to have disappeared";
                dirty.get()->insertDirtyFile(source.fileId);
                mSources.erase(it++);
                needsSave = true;
            } else {
                ++it;
            }
        }
    }
    List<std::shared_ptr<IndexerJob> > pendingJobs = std::move(mPendingJobs);
    mState = Loaded;
    if ((!dirty || !startDirtyJobs(dirty.get())) && needsSave)
        save();

    for (const auto &job : pendingJobs) {
        assert(job);
        if (!hasSource(job->source)) {
            index(job);
        }
    }
}

bool Project::load(FileManagerMode mode)
{
    switch (mState) {
    case Unloaded:
        fileManager.reset(new FileManager);
        fileManager->init(shared_from_this(),
                          mode == FileManager_Asynchronous ? FileManager::Asynchronous : FileManager::Synchronous);
        // duplicated from init
        break;
    case Inited:
        break;
    case Loading:
    case Loaded:
    case Syncing:
        return false;
    }
    mState = Loading;
    RestoreThread *thread = new RestoreThread(shared_from_this());
    thread->start();
    return true;
}

void Project::unload()
{
    switch (mState) {
    case Unloaded:
        return;
    case Syncing:
    case Loading: {
        std::weak_ptr<Project> weak = shared_from_this();
        EventLoop::eventLoop()->registerTimer([weak](int) { if (std::shared_ptr<Project> project = weak.lock()) project->unload(); },
                                              1000, Timer::SingleShot);
        return; }
    default:
        break;
    }
    for (const auto &job : mActiveJobs) {
        assert(job.second);
        Server::instance()->jobScheduler()->abort(job.second);
    }

    const String msg = sync();
    if (!msg.isEmpty())
        error() << msg;

    mActiveJobs.clear();
    fileManager.reset();

    mSymbols.clear();
    mSymbolNames.clear();
    mUsr.clear();
    mFiles.clear();
    mSources.clear();
    mVisitedFiles.clear();
    mDependencies.clear();
    mState = Unloaded;
    mSyncTimer.stop();
    mDirtyTimer.stop();
}

bool Project::match(const Match &p, bool *indexed) const
{
    Path paths[] = { p.pattern(), p.pattern() };
    paths[1].resolve();
    const int count = paths[1].compare(paths[0]) ? 2 : 1;
    bool ret = false;
    const Path resolvedPath = mPath.resolved();;
    for (int i=0; i<count; ++i) {
        const Path &path = paths[i];
        const uint32_t id = Location::fileId(path);
        if (id && isIndexed(id)) {
            if (indexed)
                *indexed = true;
            return true;
        } else if (mFiles.contains(path) || p.match(mPath) || p.match(resolvedPath)) {
            if (!indexed)
                return true;
            ret = true;
        }
    }
    if (indexed)
        *indexed = false;
    return ret;
}

void Project::onJobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexData> &indexData)
{
    mSyncTimer.stop();
    if (mState == Syncing) {
        mPendingIndexData[indexData->key] = std::make_pair(job, indexData);
        return;
    } else if (mState != Loaded) {
        return;
    }
    assert(indexData);
    std::shared_ptr<IndexerJob> restart;
    const uint32_t fileId = indexData->fileId();
    auto j = mActiveJobs.take(indexData->key);
    if (!j) {
        error() << "Couldn't find JobData for" << Location::path(fileId);
        return;
    } else if (j != job) {
        error() << "Wrong IndexerJob for for" << Location::path(fileId);
        return;
    }

    const bool success = job->flags & IndexerJob::Complete;
    assert(!(job->flags & IndexerJob::Aborted));
    assert(((job->flags & (IndexerJob::Complete|IndexerJob::Crashed)) == IndexerJob::Complete)
           || ((job->flags & (IndexerJob::Complete|IndexerJob::Crashed)) == IndexerJob::Crashed));
    const auto &options = Server::instance()->options();
    if (!success) {
        releaseFileIds(job->visited);
    }

    auto src = mSources.find(indexData->key);
    if (src == mSources.end()) {
        error() << "Can't find source for" << Location::path(fileId);
        return;
    }

    const int idx = mJobCounter - mActiveJobs.size();
    if (testLog(RTags::CompilationErrorXml)) {
        logDirect(RTags::CompilationErrorXml, indexData->xmlDiagnostics);
        if (!(options.options & Server::NoProgress)) {
            log(RTags::CompilationErrorXml,
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<progress index=\"%d\" total=\"%d\"></progress>",
                idx, mJobCounter);
        }
    }

    mIndexData[indexData->key] = indexData;
    if (success) {
        src->second.parsed = indexData->parseTime;
        error("[%3d%%] %d/%d %s %s.",
              static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
              String::formatTime(time(0), String::Time).constData(),
              indexData->message.constData());
    } else {
        assert(indexData->flags & IndexerJob::Crashed);
        error("[%3d%%] %d/%d %s %s indexing crashed.",
              static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
              String::formatTime(time(0), String::Time).constData(),
              Location::path(fileId).toTilde().constData());
    }

    if (mActiveJobs.isEmpty()) {
        mSyncTimer.restart(indexData->flags & IndexerJob::Dirty ? 0 : SyncTimeout, Timer::SingleShot);
    } else if (options.syncThreshold && mIndexData.size() >= options.syncThreshold) {
        startSync(Sync_Asynchronous);
    }
}

bool Project::save()
{
    if (!Server::instance()->saveFileIds()) {
        return false;
    }

    Path srcPath = mPath;
    RTags::encodePath(srcPath);
    const Server::Options &options = Server::instance()->options();
    const Path p = options.dataDir + srcPath;
    DataFile file(p);
    if (!file.open(DataFile::Write)) {
        error("Save error %s: %s", p.constData(), file.error().constData());
        return false;
    }
    CursorInfo::serialize(file, mSymbols);
    file << mSymbolNames << mUsr
         << mDependencies << mSources << mVisitedFiles;
    if (!file.flush()) {
        error("Save error %s: %s", p.constData(), file.error().constData());
        return false;
    }

    return true;
}

void Project::index(const std::shared_ptr<IndexerJob> &job)
{
    const Path sourceFile = job->sourceFile;
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(job->sourceFile.constData(), fileFilter)) {
        error() << "Not indexing" << job->sourceFile.constData() << "because of file filter"
                << fileFilter;
        return;
    }

    if (mState != Loaded) {
        mPendingJobs.append(job);
        return;
    }
    const uint64_t key = job->source.key();
    if (Server::instance()->suspended() && mSources.contains(key) && (job->flags & IndexerJob::Compile)) {
        return;
    }
    mSources[key] = job->source;

    std::shared_ptr<IndexerJob> &ref = mActiveJobs[key];
    if (ref) {
        releaseFileIds(ref->visited);
        Server::instance()->jobScheduler()->abort(ref);
        --mJobCounter;
    }
    ref = job;

    if (mIndexData.remove(key))
        --mJobCounter;

    if (!mJobCounter++)
        mTimer.start();

    mSyncTimer.stop();
    Server::instance()->jobScheduler()->add(job);
}

void Project::onFileModifiedOrRemoved(const Path &file)
{
    const uint32_t fileId = Location::fileId(file);
    debug() << file << "was modified" << fileId;
    if (!fileId)
        return;
    if (Server::instance()->suspended() || mSuspendedFiles.contains(fileId)) {
        warning() << file << "is suspended. Ignoring modification";
        return;
    }
    if (mPendingDirtyFiles.insert(fileId)) {
        mDirtyTimer.restart(DirtyTimeout, Timer::SingleShot);
    }
}

void Project::onDirtyTimeout(Timer *)
{
    Set<uint32_t> dirtyFiles = std::move(mPendingDirtyFiles);
    WatcherDirty dirty(mDependencies, dirtyFiles);
    startDirtyJobs(&dirty);
}

List<Source> Project::sources(uint32_t fileId) const
{
    List<Source> ret;
    if (fileId) {
        auto it = mSources.lower_bound(Source::key(fileId, 0));
        while (it != mSources.end()) {
            uint32_t f, b;
            Source::decodeKey(it->first, f, b);
            if (f != fileId)
                break;
            ret.append(it->second);
            ++it;
        }
    }
    return ret;
}

void Project::addDependencies(const DependencyMap &deps, Set<uint32_t> &newFiles)
{
    StopWatch timer;

    const auto end = deps.end();
    for (auto it = deps.begin(); it != end; ++it) {
        Set<uint32_t> &values = mDependencies[it->first];
        if (values.isEmpty()) {
            values = it->second;
        } else {
            values.unite(it->second);
        }
        if (newFiles.isEmpty()) {
            newFiles = it->second;
        } else {
            newFiles.unite(it->second);
        }
        newFiles.insert(it->first);
    }
}

Set<uint32_t> Project::dependencies(uint32_t fileId, DependencyMode mode) const
{
    if (mode == DependsOnArg)
        return mDependencies.value(fileId);

    Set<uint32_t> ret;
    const auto end = mDependencies.end();
    for (auto it = mDependencies.begin(); it != end; ++it) {
        if (it->second.contains(fileId))
            ret.insert(it->first);
    }
    return ret;
}

int Project::reindex(const Match &match, const std::shared_ptr<QueryMessage> &query)
{
    if (query->type() == QueryMessage::Reindex) {
        Set<uint32_t> dirtyFiles;

        const auto end = mDependencies.constEnd();
        for (auto it = mDependencies.constBegin(); it != end; ++it) {
            if (!dirtyFiles.contains(it->first) && (match.isEmpty() || match.match(Location::path(it->first)))) {
                dirtyFiles.insert(it->first);
            }
        }
        if (dirtyFiles.isEmpty())
            return 0;
        SimpleDirty dirty;
        dirty.init(dirtyFiles, mDependencies);
        return startDirtyJobs(&dirty, query->unsavedFiles());
    } else {
        assert(query->type() == QueryMessage::CheckReindex);
        IfModifiedDirty dirty(mDependencies, match);
        return startDirtyJobs(&dirty, query->unsavedFiles());
    }
}

int Project::remove(const Match &match)
{
    int count = 0;
    Set<uint32_t> dirty;
    auto it = mSources.begin();
    while (it != mSources.end()) {
        if (match.match(it->second.sourceFile())) {
            const uint32_t fileId = it->second.fileId;
            mSources.erase(it++);
            std::shared_ptr<IndexerJob> job = mActiveJobs.take(fileId);
            if (job) {
                releaseFileIds(job->visited);
                Server::instance()->jobScheduler()->abort(job);
            }
            mIndexData.remove(fileId);
            dirty.insert(fileId);
            ++count;
        } else {
            ++it;
        }
    }
    if (count) {
        RTags::dirtySymbols(mSymbols, dirty);
        RTags::dirtySymbolNames(mSymbolNames, dirty);
        RTags::dirtyUsr(mUsr, dirty);
    }
    return count;
}

int Project::startDirtyJobs(Dirty *dirty, const UnsavedFiles &unsavedFiles)
{
    List<Source> toIndex;
    for (const auto &source : mSources) {
        if (dirty->isDirty(source.second)) {
            toIndex << source.second;
        }
    }
    const Set<uint32_t> dirtyFiles = dirty->dirtied();

    for (const auto &fileId : dirtyFiles) {
        mVisitedFiles.remove(fileId);
    }

    for (const auto &source : toIndex) {
        index(std::shared_ptr<IndexerJob>(new IndexerJob(source, IndexerJob::Dirty, mPath, unsavedFiles)));
    }

    if (!toIndex.size() && !dirtyFiles.isEmpty()) {
        // this is for the case where we've removed a file
        RTags::dirtySymbols(mSymbols, dirtyFiles);
        RTags::dirtySymbolNames(mSymbolNames, dirtyFiles);
        RTags::dirtyUsr(mUsr, dirtyFiles);
    } else {
        mDirtyFiles += dirtyFiles;
    }
    return toIndex.size();
}

static inline int writeSymbolNames(const SymbolNameMap &symbolNames, SymbolNameMap &current)
{
    int ret = 0;
    auto it = symbolNames.begin();
    const auto end = symbolNames.end();
    while (it != end) {
        Set<Location> &value = current[it->first];
        int count = 0;
        value.unite(it->second, &count);
        ret += count;
        ++it;
    }
    return ret;
}

static inline void joinCursors(SymbolMap &symbols, const Set<Location> &locations)
{
    for (auto it = locations.begin(); it != locations.end(); ++it) {
        const auto c = symbols.find(*it);
        if (c != symbols.constEnd()) {
            std::shared_ptr<CursorInfo> &cursorInfo = c->second;
            for (auto innerIt = locations.begin(); innerIt != locations.end(); ++innerIt) {
                if (innerIt != it)
                    cursorInfo->targets.insert(*innerIt);
            }
            // ### this is filthy, we could likely think of something better
        }
    }
}

static inline void writeUsr(const UsrMap &usr, UsrMap &current, SymbolMap &symbols)
{
    auto it = usr.begin();
    const auto end = usr.end();
    while (it != end) {
        Set<Location> &value = current[it->first];
        int count = 0;
        value.unite(it->second, &count);
        if (count && value.size() > 1)
            joinCursors(symbols, value);
        ++it;
    }
}

static inline void resolvePendingReferences(SymbolMap& symbols, const UsrMap& usrs, const UsrMap& refs)
{
    for (const auto &ref : refs) {
        assert(!ref.second.isEmpty());
        // find the declaration
        List<String> refUsrs;
        {
            String refUsr = ref.first;
            refUsrs.append(refUsr);
            // assume this is an implicit instance method for a property, replace the last (im) with (py)
            const int lastIm = refUsr.lastIndexOf("(im)");
            if (lastIm != -1) {
                refUsr.replace(lastIm, 4, "(py)");
                refUsrs.append(refUsr);
            }
        }
        SymbolMap targets;
        for (const String& refUsr : refUsrs) {
            const auto usr = usrs.find(refUsr);
            if (usr != usrs.end()) {
                for (const Location& usrLoc : usr->second) {
                    auto symbol = symbols.value(usrLoc);
                    assert(symbol);
                    if (RTags::isCursor(symbol->kind))
                        targets[usrLoc] = symbol;
                }
            }
        }
        if (!targets.isEmpty()) {
            for (const auto &r : ref.second) {
                auto &referenceCursor = symbols[r];
                assert(referenceCursor);
                for (const auto &t : targets) {
                    referenceCursor->targets.insert(t.first);
                    t.second->references.insert(r);
                }
            }
        }
    }
}

static inline int writeSymbols(SymbolMap &symbols, SymbolMap &current)
{
    int ret = 0;
    if (!symbols.isEmpty()) {
        if (current.isEmpty()) {
            current = symbols;
            ret = symbols.size();
        } else {
            auto it = symbols.begin();
            const auto end = symbols.end();
            while (it != end) {
                auto cur = current.find(it->first);
                if (cur == current.end()) {
                    current[it->first] = it->second;
                    ++ret;
                } else {
                    if (cur->second->unite(it->second))
                        ++ret;
                }
                ++it;
            }
        }
    }
    return ret;
}

bool Project::isIndexed(uint32_t fileId) const
{
    if (mVisitedFiles.contains(fileId))
        return true;

    const uint64_t key = Source::key(fileId, 0);
    auto it = mSources.lower_bound(key);
    if (it != mSources.end()) {
        uint32_t f, b;
        Source::decodeKey(it->first, f, b);
        if (f == fileId)
            return true;
    }
    return false;
}

const Set<uint32_t> &Project::suspendedFiles() const
{
    return mSuspendedFiles;
}

void Project::clearSuspendedFiles()
{
    mSuspendedFiles.clear();
}

bool Project::toggleSuspendFile(uint32_t file)
{
    if (!mSuspendedFiles.insert(file)) {
        mSuspendedFiles.remove(file);
        return false;
    }
    return true;
}

bool Project::isSuspended(uint32_t file) const
{
    return mSuspendedFiles.contains(file);
}

void Project::addFixIts(const DependencyMap &visited, const FixItMap &fixIts) // lock always held
{
    for (auto it = visited.begin(); it != visited.end(); ++it) {
        const auto fit = fixIts.find(it->first);
        if (fit == fixIts.end()) {
            mFixIts.erase(it->first);
        } else {
            mFixIts[it->first] = fit->second;
        }
    }
}

String Project::fixIts(uint32_t fileId) const
{
    const auto it = mFixIts.find(fileId);
    String out;
    if (it != mFixIts.end()) {
        const Set<FixIt> &fixIts = it->second;
        if (!fixIts.isEmpty()) {
            auto f = fixIts.end();
            do {
                --f;
                if (!out.isEmpty())
                    out.append('\n');
                out.append(String::format<32>("%d:%d %d %s", f->line, f->column, f->length, f->text.constData()));

            } while (f != fixIts.begin());
        }
    }
    return out;
}

bool Project::startSync(SyncMode mode)
{
    if (mState != Loaded) {
        if (mode == Sync_Asynchronous)
            mSyncTimer.restart(SyncTimeout, Timer::SingleShot);
        return false;
    }
    assert(mState == Loaded);
    mState = Syncing;
    mSyncTimer.stop();
    if (mode == Sync_Synchronous) {
        const String msg = sync();
        if (!msg.isEmpty())
            error() << msg;
        onSynced();
    } else {
        SyncThread *thread = new SyncThread(shared_from_this());
        thread->start();
    }
    return true;
}

void Project::reloadFileManager()
{
    fileManager->reload(FileManager::Asynchronous);
}

static inline bool checkFunction(unsigned int kind)
{
    switch (kind) {
    case CXCursor_VarDecl:
    case CXCursor_ParmDecl:
        return true;
    default:
        break;
    }
    return false;
}

static inline bool matchSymbolName(const String &needle, const String &haystack, bool checkFunction)
{
    int start = 0;
    if (checkFunction) {
        // we generate symbols for arguments and local variables in functions
        // . E.g. there's a symbol with the symbolName:
        // bool matchSymbolName(String &, String &, bool)::checkFunction
        // we don't want to match when we're searching for "matchSymbolName" so
        // we start searching at the index of ):: if we're a function. That is
        // unless you really sent in an exact match. In that case you deserve a
        // hit.
        if (needle == haystack)
            return true;

        start = haystack.indexOf(")::");
        if (start != -1) {
            start += 2;
        } else {
            start = 0;
        }
    }
    // We automagically generate symbols with stripped argument lists
    if (!strncmp(needle.constData(), haystack.constData() + start, needle.size())
        && (haystack.size() - start == needle.size() || haystack.at(start + needle.size()) == '(')) {
        return true;
    }
    return false;
}

Set<Location> Project::locations(const String &symbolName, uint32_t fileId) const
{
    Set<Location> ret;
    if (fileId) {
        const SymbolMap s = symbols(fileId);
        for (auto it = s.begin(); it != s.end(); ++it) {
            if (!RTags::isReference(it->second->kind)
                && (symbolName.isEmpty() || matchSymbolName(symbolName, it->second->symbolName, checkFunction(it->second->kind)))) {
                ret.insert(it->first);
            }
        }
    } else if (symbolName.isEmpty()) {
        for (auto it = mSymbols.constBegin(); it != mSymbols.constEnd(); ++it) {
            if (!RTags::isReference(it->second->kind))
                ret.insert(it->first);
        }
    } else {
        auto it = mSymbolNames.lower_bound(symbolName);
        while (it != mSymbolNames.end() && it->first.startsWith(symbolName)) {
            if (matchSymbolName(symbolName, it->first, true)) // assume function
                ret.unite(it->second);
            ++it;
        }
    }
    return ret;
}

List<RTags::SortedCursor> Project::sort(const Set<Location> &locations, unsigned int flags) const
{
    List<RTags::SortedCursor> sorted;
    sorted.reserve(locations.size());
    for (auto it = locations.begin(); it != locations.end(); ++it) {
        RTags::SortedCursor node(*it);
        const auto found = mSymbols.find(*it);
        if (found != mSymbols.end()) {
            node.isDefinition = found->second->isDefinition();
            if (flags & Sort_DeclarationOnly && node.isDefinition) {
                const std::shared_ptr<CursorInfo> decl = found->second->bestTarget(mSymbols);
                if (decl && !decl->isNull())
                    continue;
            }
            node.kind = found->second->kind;
        }
        sorted.push_back(node);
    }

    if (flags & Sort_Reverse) {
        std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedCursor>());
    } else {
        std::sort(sorted.begin(), sorted.end());
    }
    return sorted;
}

SymbolMap Project::symbols(uint32_t fileId) const
{
    SymbolMap ret;
    if (fileId) {
        for (auto it = mSymbols.lower_bound(Location(fileId, 1, 0));
             it != mSymbols.end() && it->first.fileId() == fileId; ++it) {
            ret[it->first] = it->second;
        }
    }
    return ret;
}

void Project::watch(const Path &file)
{
    Path dir = file.parentDir();
    if (dir.isEmpty()) {
        error() << "Got empty parent dir for" << file;
    } else {
        if (mWatchedPaths.contains(dir))
            return;
        dir.resolve();
        if (((Server::instance()->options().options & Server::WatchSystemPaths) || !dir.isSystem())
            && mWatchedPaths.insert(dir)) {
            mWatcher.watch(dir);
        }
    }
}

bool Project::hasSource(const Source &source) const
{
    const uint64_t key = source.key();
    auto it = mSources.lower_bound(Source::key(source.fileId, 0));
    const auto &options = Server::instance()->options();
    const bool disallowMultiple = options.options & Server::DisallowMultipleSources;
    const bool ignoreExistingFiles = options.options & Server::NoFileSystemWatch;
    bool found = false;
    while (it != mSources.end()) {
        uint32_t f, b;
        Source::decodeKey(it->first, f, b);
        if (f != source.fileId) {
            break;
        }
        found = true;
        if (ignoreExistingFiles) {
            // When we're not watching the file system, we ignore
            // updating compiles. This means that you always have to
            // do check-reindex to build existing files!
            return true;
        }
        if (it->first == key) {
            return it->second.compareArguments(source);
            // if the build key is the same we want to return false if the arguments have updated
        }
        if (disallowMultiple || it->second.compareArguments(source)) {// similar enough that we don't want two builds
            return true;
        }
        ++it;
    }
    if (found) {
        const Map<String, String> config = RTags::rtagsConfig(source.sourceFile());
        if (config.contains("multi-build")) {
            const List<String> multi = config.value("multi-build").split(';');
            const Path sourceFile = source.sourceFile();
            for (const auto &filter : multi) {
                if (!fnmatch(filter.constData(), sourceFile.constData(), 0)) {
                    // we allow multi builds for this file
                    return false;
                }
            }
            return true;
        }
    }

    return false;
}

void Project::onSynced()
{
    assert(mState == Syncing);
    mState = Loaded;
    for (const auto &it : mPendingIndexData) {
        onJobFinished(it.second.first, it.second.second);
    }
    mPendingIndexData.clear();
    for (const auto &job : mPendingJobs) {
        index(job);
    }
    mPendingJobs.clear();
}

String Project::sync()
{
    mJobCounter = mActiveJobs.size();
    StopWatch sw;
    if (mDirtyFiles.isEmpty() && mIndexData.isEmpty()) {
        return String();
    }

    if (!mDirtyFiles.isEmpty()) {
        RTags::dirtySymbols(mSymbols, mDirtyFiles);
        RTags::dirtySymbolNames(mSymbolNames, mDirtyFiles);
        RTags::dirtyUsr(mUsr, mDirtyFiles);
        mDirtyFiles.clear();
    }
    const int dirtyTime = sw.restart();

    Set<uint32_t> newFiles;
    List<UsrMap*> pendingReferences;
    int symbols = 0;
    int symbolNames = 0;
    for (auto it = mIndexData.begin(); it != mIndexData.end(); ++it) {
        const std::shared_ptr<IndexData> &data = it->second;
        addDependencies(data->dependencies, newFiles);
        addFixIts(data->dependencies, data->fixIts);
        if (!data->pendingReferenceMap.isEmpty())
            pendingReferences.append(&data->pendingReferenceMap);
        symbols += writeSymbols(data->symbols, mSymbols);
        writeUsr(data->usrMap, mUsr, mSymbols);
        symbolNames += writeSymbolNames(data->symbolNames, mSymbolNames);
    }

    for (const UsrMap *map : pendingReferences)
        resolvePendingReferences(mSymbols, mUsr, *map);

    for (auto it = newFiles.constBegin(); it != newFiles.constEnd(); ++it) {
        watch(Location::path(*it));
    }
    const int syncTime = sw.restart();
    save();
    const int saveTime = sw.elapsed();
    double timerElapsed = (mTimer.elapsed() / 1000.0);
    const double averageJobTime = timerElapsed / mIndexData.size();
    const String msg = String::format<1024>("Jobs took %.2fs, %sdirtying took %.2fs, "
                                            "syncing took %.2fs, saving took %.2fs. We're using %lldmb of memory. "
                                            "%d symbols, %d symbolNames", timerElapsed,
                                            mIndexData.size() > 1 ? String::format("(avg %.2fs), ", averageJobTime).constData() : "",
                                            dirtyTime / 1000.0, syncTime / 1000.0, saveTime / 1000.0, MemoryMonitor::usage() / (1024 * 1024),
                                            symbols, symbolNames);
    mIndexData.clear();
    mTimer.start();
    return msg;
}
