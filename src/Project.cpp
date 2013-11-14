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
#include "IndexerJob.h"
#include "RTags.h"
#include "Server.h"
#include "Server.h"
#include "Cpp.h"
#include "WrapperJob.h"
#include <math.h>
#include <rct/Log.h>
#include <rct/MemoryMonitor.h>
#include <rct/Path.h>
#include <rct/Rct.h>
#include <rct/ReadLocker.h>
#include <rct/RegExp.h>
#include <rct/Thread.h>

static void *ModifiedFiles = &ModifiedFiles;
static void *Sync = &Sync;

enum {
    SyncTimeout = 500
};

class RestoreThread : public Thread
{
public:
    RestoreThread(const std::shared_ptr<Project> &project)
        : mProject(project)
    {
        setAutoDelete(true);
    }

    virtual void run()
    {
        std::shared_ptr<Project> project = mProject.lock();
        if (project) {
            restore(project);
            EventLoop::mainEventLoop()->callLater(std::bind(&Project::restore, project.get(), std::placeholders::_1), this);
        }
    }
    void restore(const std::shared_ptr<Project> &project)
    {
        assert(project->state() == Project::Loading);
        StopWatch timer;
        Path path = project->path();
        RTags::encodePath(path);
        const Path p = Server::instance()->options().dataDir + path;
        bool restoreError = false;
        FILE *f = fopen(p.constData(), "r");
        if (!f) {
            return;
        }

        Deserializer in(f);
        int version;
        in >> version;
        if (version != Server::DatabaseVersion) {
            error("Wrong database version. Expected %d, got %d for %s. Removing.", Server::DatabaseVersion, version, p.constData());
            restoreError = true;
            goto end;
        }
        {
            int fs;
            in >> fs;
            if (fs != Rct::fileSize(f)) {
                error("%s seems to be corrupted, refusing to restore %s",
                      p.constData(), path.constData());
                restoreError = true;
                goto end;
            }
        }
        in >> mSymbols >> mSymbolNames >> mUsr >> mDependencies >> mSources >> mVisitedFiles;
  end:
        fclose(f);

        if (restoreError) {
            Path::rm(p);
        } else {
            error() << "Restored project" << path << "in" << timer.elapsed() << "ms";
        }
    }

    std::weak_ptr<Project> mProject;
    SymbolMap mSymbols;
    SymbolNameMap mSymbolNames;
    UsrMap mUsr;
    DependencyMap mDependencies;
    SourceMap mSources;
    Set<uint32_t> mVisitedFiles;
};

Project::Project(const Path &path)
    : mPath(path), mState(Unloaded), mJobCounter(0)
{
    mWatcher.modified().connect(std::bind(&Project::dirty, this, std::placeholders::_1));
    mWatcher.removed().connect(std::bind(&Project::dirty, this, std::placeholders::_1));
    if (Server::instance()->options().options & Server::NoFileManagerWatch) {
        mWatcher.removed().connect(std::bind(&Project::reloadFileManager, this));
        mWatcher.added().connect(std::bind(&Project::reloadFileManager, this));
    }
    mSyncTimer.timeout().connect(std::bind(&Project::onTimerFired, this, std::placeholders::_1));
}

Project::~Project()
{
    unload();
}


void Project::init()
{
    assert(mState == Unloaded);
    mState = Inited;
    fileManager.reset(new FileManager);
    fileManager->init(shared_from_this(), FileManager::Asynchronous);
}

void Project::restore(RestoreThread *thread)
{
    if (state() != Loading)
        return;

    mSymbols = std::move(thread->mSymbols);
    mSymbolNames = std::move(thread->mSymbolNames);
    mUsr = std::move(thread->mUsr);;
    mDependencies = std::move(thread->mDependencies);
    mSources = std::move(thread->mSources);

    mVisitedFiles = std::move(thread->mVisitedFiles);

    DependencyMap reversedDependencies;
    Set<uint32_t> dirty;
    // these dependencies are in the form of:
    // Path.cpp: Path.h, String.h ...
    // mDependencies are like this:
    // Path.h: Path.cpp, Server.cpp ...

    bool needsSave = false;
    {
        auto it = mDependencies.begin();
        while (it != mDependencies.end()) {
            const Path file = Location::path(it->first);
            if (!file.exists()) {
                error() << "File doesn't exist" << file;
                mDependencies.erase(it++);
                needsSave = true;
                continue;
            }
            watch(file);
            for (auto s = it->second.constBegin(); s != it->second.constEnd(); ++s)
                reversedDependencies[*s].insert(it->first);
            ++it;
        }
    }

    auto it = mSources.begin();
    while (it != mSources.end()) {
        const Source &source = it->second;
        if (!source.sourceFile().isFile()) {
            error() << source.sourceFile() << "seems to have disappeared";
            dirty.insert(source.fileId);
            mSources.erase(it++);
            needsSave = true;
        } else {
            assert(mDependencies.value(source.fileId).contains(source.fileId));
            const Set<uint32_t> &deps = reversedDependencies[source.fileId];
            // error() << source.sourceFile() << "has" << deps.size();
            for (auto d = deps.constBegin(); d != deps.constEnd(); ++d) {
                if (!dirty.contains(*d) && Location::path(*d).lastModifiedMs() > source.parsed) {
                    dirty.insert(*d);
                    // error() << Location::path(*d).lastModifiedMs() << "is more than" << source.parsed
                    //         << it->second.sourceFile() << Location::path(*d)
                    //         << String::formatTime(source.parsed / 1000)
                    //         << String::formatTime(Location::path(*d).lastModifiedMs() / 1000);
                }
            }
            ++it;
        }
    }
    Hash<uint64_t, JobData> pendingJobs = std::move(mJobs);
    mState = Loaded;
    if (!dirty.isEmpty()) {
        startDirtyJobs(dirty);
    } else if (needsSave) {
        save();
    }
    for (auto it = pendingJobs.constBegin(); it != pendingJobs.constEnd(); ++it) {
        assert(!it->second.pendingSource.isNull());
        assert(it->second.pendingType != IndexerJob::Invalid);
        std::shared_ptr<Cpp> cpp = RTags::preprocess(it->second.pendingSource);
        if (!cpp) {
            error() << "Unable to preprocess" << it->second.pendingSource.sourceFile();
        } else {
            index(it->second.pendingSource, it->second.pendingType, cpp);
        }
    }
}

void Project::load(FileManagerMode mode)
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
        return;
    }
    mState = Loading;
    RestoreThread *thread = new RestoreThread(shared_from_this());
    thread->start();
}

void Project::unload()
{
    for (auto it = mJobs.constBegin(); it != mJobs.constEnd(); ++it) {
        if (it->second.job)
            it->second.job->abort();
    }

    mJobs.clear();
    fileManager.reset();

    mSymbols.clear();
    mSymbolNames.clear();
    mUsr.clear();
    mFiles.clear();
    mSources.clear();
    mVisitedFiles.clear();
    mDependencies.clear();
    mState = Unloaded;
}

bool Project::match(const Match &p, bool *indexed) const
{
    Path paths[] = { p.pattern(), p.pattern() };
    paths[1].resolve();
    const int count = paths[1].compare(paths[0]) ? 2 : 1;
    bool ret = false;
    for (int i=0; i<count; ++i) {
        const Path &path = paths[i];
        const uint32_t id = Location::fileId(path);
        if (isIndexed(id)) {
            if (indexed)
                *indexed = true;
            return true;
        } else if (mFiles.contains(path) || p.match(mPath)) {
            if (!indexed)
                return true;
            ret = true;
        }
    }
    if (indexed)
        *indexed = false;
    return ret;
}

void Project::onJobFinished(const std::shared_ptr<IndexData> &indexData)
{
    assert(indexData);
    Source pending;
    std::shared_ptr<Cpp> pendingCpp;
    IndexerJob::IndexType pendingType = IndexerJob::Invalid;
    bool syncNow = false;
    const uint32_t fileId = indexData->fileId();
    if (indexData->type == IndexerJob::Dump) {
        bool found = false;
        Connection *conn = mDumps.take(indexData->key, &found);
        if (!found) {
            error() << "Couldn't find JobData for" << Location::path(fileId);
            return;
        }
        if (conn) {
            conn->write(indexData->message);
            conn->finish();
        }
        return;
    }

    const auto it = mJobs.find(indexData->key);
    if (it == mJobs.end()) {
        error() << "Couldn't find JobData for" << Location::path(fileId);
        // not sure if this can happen when unloading while jobs are running
        return;
    }

    JobData *jobData = &it->second;
    assert(jobData->job);
    const bool success = !indexData->aborted && jobData->job->state != IndexerJob::Aborted;
    if (indexData->aborted) {
        ++jobData->crashCount;
    } else {
        jobData->crashCount = 0;
    }

    enum { MaxCrashCount = 5 }; // ### configurable?
    if (jobData->crashCount < MaxCrashCount) {
        if (jobData->pendingType != IndexerJob::Invalid) {
            assert(jobData->job->state == IndexerJob::Aborted);
            std::swap(pendingType, jobData->pendingType);
            std::swap(pending, jobData->pendingSource);
            std::swap(pendingCpp, jobData->pendingCpp);
        } else if (!success) {
            pending = jobData->job->source;
            pendingType = jobData->job->type;
            pendingCpp = jobData->job->cpp;
            if (jobData->job->state != IndexerJob::Aborted) {
                // ### we should maybe wait a little before restarting or something.
                error("%s crashed, restarting", jobData->job->source.sourceFile().constData());
            }
        }
    }
    if (pendingType == IndexerJob::Invalid) {
        const int idx = mJobCounter - mJobs.size() + 1;
        if (testLog(RTags::CompilationErrorXml)) {
            logDirect(RTags::CompilationErrorXml, indexData->xmlDiagnostics);
            log(RTags::CompilationErrorXml,
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<progress index=\"%d\" total=\"%d\"></progress>",
                idx, mJobCounter);
        }
        if (success) {
            auto src = mSources.find(indexData->key);
            if (src != mSources.end()) {
                mPendingData[indexData->key] = indexData;
                src->second.parsed = indexData->parseTime;
                error("[%3d%%] %d/%d %s %s.",
                      static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                      String::formatTime(time(0), String::Time).constData(),
                      indexData->message.constData());
            }
        } else {
            assert(indexData->aborted);
            error("[%3d%%] %d/%d %s %s indexing crashed.",
                  static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                  String::formatTime(time(0), String::Time).constData(),
                  Location::path(fileId).toTilde().constData());
        }
        jobData = 0;
        mJobs.erase(it);

        const int syncThreshold = Server::instance()->options().syncThreshold;
        if (mJobs.isEmpty()) {
            mSyncTimer.restart(indexData->type == IndexerJob::Dirty ? 0 : SyncTimeout, Timer::SingleShot);
        } else if (syncThreshold && mPendingData.size() >= syncThreshold) {
            syncNow = true;
        }
    } else {
        jobData->job.reset();
    }
    if (syncNow)
        sync();
    if (pendingType != IndexerJob::Invalid) {
        assert(!pending.isNull());
        index(pending, pendingType, pendingCpp);
        --mJobCounter;
    }
}

bool Project::save()
{
    if (!Server::instance()->saveFileIds())
        return false;

    Path srcPath = mPath;
    RTags::encodePath(srcPath);
    const Server::Options &options = Server::instance()->options();
    const Path p = options.dataDir + srcPath;
    FILE *f = fopen(p.constData(), "w");
    if (!f) {
        error("Can't open file %s", p.constData());
        return false;
    }
    Serializer out(f);
    out << static_cast<int>(Server::DatabaseVersion);
    const int pos = ftell(f);
    out << static_cast<int>(0) << mSymbols << mSymbolNames << mUsr
        << mDependencies << mSources << mVisitedFiles;

    const int size = ftell(f);
    fseek(f, pos, SEEK_SET);
    out << size;

    fclose(f);
    return true;
}

void Project::dump(const Source &source, Connection *conn)
{
    if (source.isNull()) {
        conn->write<64>("No source information for %s", source.sourceFile().constData());
        conn->finish();;
        return;
    }

    Connection *&c = mDumps[source.fileId];
    if (c) {
        conn->write<64>("%s is being dumped as we speak", source.sourceFile().constData());
        conn->finish();
        return;
    }
    c = conn;
    std::shared_ptr<Cpp> cpp = RTags::preprocess(source);
    if (!cpp) {
        conn->write<64>("Unable to preprocess %s", source.sourceFile().constData());
        conn->finish();
        return;
    }

    std::shared_ptr<IndexerJob> job(new IndexerJob(IndexerJob::Dump, mPath, source, cpp));
    if (!job) {
        mDumps.remove(source.fileId);
        conn->write<64>("Couldn't create dump job for %s", source.sourceFile().constData());
        conn->finish();
        return;
    }
    job->startLocal();
}

void Project::index(const Source &source, IndexerJob::IndexType type, const std::shared_ptr<Cpp> &cpp)
{
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(source.sourceFile().constData(), fileFilter))
        return;

    const uint64_t key = source.key();
    JobData &data = mJobs[key];
    if (mState != Loaded) {
        // error() << "Index called at" << static_cast<int>(mState) << "time. Setting pending" << source.sourceFile();
        data.pendingSource = source;
        data.pendingType = type;
        data.pendingCpp = cpp;
        return;
    }
    if (data.job) {
        // error() << "There's already something here for" << source.sourceFile();
        if (!data.job->update(type, source, cpp)) {
            // error() << "Aborting and setting pending" << source.sourceFile();
            data.pendingSource = source;
            data.pendingType = type;
            data.pendingCpp = cpp;
        }
        return;
    }

    mSources[key] = source;
    watch(source.sourceFile());

    data.pendingSource.clear();
    data.pendingType = IndexerJob::Invalid;
    data.pendingCpp.reset();
    mPendingData.remove(key);

    if (!mJobCounter++)
        mTimer.start();

    data.job.reset(new IndexerJob(type, mPath, source, cpp));
    mSyncTimer.stop();
    Server::instance()->startJob(data.job);
}

bool Project::index(const Source &source, const std::shared_ptr<Cpp> &cpp)
{
    assert(!source.isNull());

    if (hasSource(source)) {
        debug() << source.sourceFile() << " is not dirty. ignoring";
        return false;
    }

    index(source, IndexerJob::Makefile, cpp);
    return true;
}

void Project::dirty(const Path &file)
{
    const uint32_t fileId = Location::fileId(file);
    if (mSuspendedFiles.contains(fileId)) {
        warning() << file << "is suspended. Ignoring modification";
        return;
    }
    debug() << file << "was modified" << fileId;
    if (fileId) {
        Set<uint32_t> dirty;
        dirty.insert(fileId);
        startDirtyJobs(dirty);
    }
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

int Project::reindex(const Match &match)
{
    Set<uint32_t> dirty;

    const auto end = mDependencies.constEnd();
    for (auto it = mDependencies.constBegin(); it != end; ++it) {
        if (match.isEmpty() || match.match(Location::path(it->first))) {
            dirty.insert(it->first);
        }
    }
    if (dirty.isEmpty())
        return 0;
    startDirtyJobs(dirty);
    return dirty.size();
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
            JobData data = mJobs.take(fileId);
            if (data.job)
                data.job->abort();
            mPendingData.remove(fileId);
            dirty.insert(fileId);
            ++count;
        } else {
            ++it;
        }
    }
    if (count)
        startDirtyJobs(dirty);
    return count;
}

void Project::startDirtyJobs(const Set<uint32_t> &dirty)
{
    Set<uint32_t> dirtyFiles;
    for (auto it = dirty.constBegin(); it != dirty.constEnd(); ++it) {
        const Set<uint32_t> deps = mDependencies.value(*it);
        dirtyFiles.insert(*it);
        if (!deps.isEmpty())
            dirtyFiles += deps;
    }
    mVisitedFiles -= dirtyFiles;

    bool indexed = false;
    for (auto it = dirtyFiles.constBegin(); it != dirtyFiles.constEnd(); ++it) {
        auto src = mSources.lower_bound(Source::key(*it, 0));
        while (src != mSources.end()) {
            uint32_t f, b;
            Source::decodeKey(src->first, f, b);
            // error() << "Decoded" << Location::path(f);
            if (f != *it)
                break;
#warning this preprocessing should happen in a job
            std::shared_ptr<Cpp> cpp = RTags::preprocess(src->second);
            if (!cpp) {
                error() << "Couldn't preprocess" << src->second.sourceFile();
            } else {
                index(src->second, IndexerJob::Dirty, cpp);
                indexed = true;
            }
            ++src;
        }
    }
    if (!indexed && !dirtyFiles.isEmpty()) {
        RTags::dirtySymbols(mSymbols, dirtyFiles);
        RTags::dirtySymbolNames(mSymbolNames, dirtyFiles);
        RTags::dirtyUsr(mUsr, dirtyFiles);
    } else {
        mPendingDirtyFiles += dirtyFiles;
    }
}

static inline void writeSymbolNames(const SymbolNameMap &symbolNames, SymbolNameMap &current)
{
    auto it = symbolNames.begin();
    const auto end = symbolNames.end();
    while (it != end) {
        Set<Location> &value = current[it->first];
        value.unite(it->second);
        ++it;
    }
}

static inline void joinCursors(SymbolMap &symbols, const Set<Location> &locations)
{
    for (auto it = locations.begin(); it != locations.end(); ++it) {
        const auto c = symbols.find(*it);
        if (c != symbols.constEnd()) {
            CursorInfo &cursorInfo = c->second;
            for (auto innerIt = locations.begin(); innerIt != locations.end(); ++innerIt) {
                if (innerIt != it)
                    cursorInfo.targets.insert(*innerIt);
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

static inline void writeSymbols(SymbolMap &symbols, SymbolMap &current)
{
    if (!symbols.isEmpty()) {
        if (current.isEmpty()) {
            current = symbols;
        } else {
            auto it = symbols.begin();
            const auto end = symbols.end();
            while (it != end) {
                auto cur = current.find(it->first);
                if (cur == current.end()) {
                    current[it->first] = it->second;
                } else {
                    cur->second.unite(it->second);
                }
                ++it;
            }
        }
    }
}

static inline void writeReferences(const ReferenceMap &references, SymbolMap &symbols)
{
    const auto end = references.end();
    for (auto it = references.begin(); it != end; ++it) {
        const Set<Location> &refs = it->second;
        for (auto rit = refs.begin(); rit != refs.end(); ++rit) {
            CursorInfo &ci = symbols[*rit];
            ci.references.insert(it->first);
        }
    }
}

void Project::syncDB(int *dirty, int *sync)
{
    StopWatch sw;
    if (mPendingDirtyFiles.isEmpty() && mPendingData.isEmpty()) {
        *dirty = 0;
        *sync = 0;
        return;
    }

    if (!mPendingDirtyFiles.isEmpty()) {
        RTags::dirtySymbols(mSymbols, mPendingDirtyFiles);
        RTags::dirtySymbolNames(mSymbolNames, mPendingDirtyFiles);
        RTags::dirtyUsr(mUsr, mPendingDirtyFiles);
        mPendingDirtyFiles.clear();
    }
    *dirty = sw.restart();

    Set<uint32_t> newFiles;
    for (auto it = mPendingData.begin(); it != mPendingData.end(); ++it) {
        const std::shared_ptr<IndexData> &data = it->second;
        addDependencies(data->dependencies, newFiles);
        addFixIts(data->dependencies, data->fixIts);
        writeSymbols(data->symbols, mSymbols);
        writeUsr(data->usrMap, mUsr, mSymbols);
        writeReferences(data->references, mSymbols);
        writeSymbolNames(data->symbolNames, mSymbolNames);
    }
    for (auto it = newFiles.constBegin(); it != newFiles.constEnd(); ++it) {
        watch(Location::path(*it));
    }
    mPendingData.clear();
    *sync = sw.elapsed();
}

bool Project::isIndexed(uint32_t fileId) const
{
    return mVisitedFiles.contains(fileId) || mSources.find(fileId) != mSources.end();
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
                out.append(String::format<32>("%d:%d(%d) %s", f->line, f->column, f->length, f->text.constData()));

            } while (f != fixIts.begin());
        }
    }
    return out;
}

void Project::onTimerFired(Timer* timer)
{
    if (timer == &mSyncTimer) {
        sync();
    } else {
        assert(0 && "Unexpected timer event in Project");
        timer->stop();
    }
}

void Project::sync()
{
    mSyncTimer.stop();
    int dirtyTime, syncTime;
    mJobCounter -= mPendingData.size();
    syncDB(&dirtyTime, &syncTime);
    StopWatch sw;
    save();
    const int saveTime = sw.elapsed();
    error() << "Jobs took" << (static_cast<double>(mTimer.elapsed()) / 1000.0)
            << "secs, dirtying took"
            << (static_cast<double>(dirtyTime) / 1000.0) << "secs, syncing took"
            << (static_cast<double>(syncTime) / 1000.0) << " secs, saving took"
            << (static_cast<double>(saveTime) / 1000.0) << " secs, using"
            << MemoryMonitor::usage() / (1024.0 * 1024.0) << "mb of memory";
    mTimer.start();
}

void Project::reloadFileManager()
{
    fileManager->reload(FileManager::Asynchronous);
}

static inline bool matchSymbolName(const String &needle, const String &haystack)
{
    if (haystack.startsWith(needle)) {
        if (haystack.size() == needle.size()) {
            return true;
        } else if ((haystack.at(needle.size()) == '<' || haystack.at(needle.size()) == '(')
                   && haystack.indexOf(")::", needle.size()) == -1) { // we don't want to match foobar for void foobar(int)::parm
            return true;
        }
    }
    return false;
}

Set<Location> Project::locations(const String &symbolName, uint32_t fileId) const
{
    Set<Location> ret;
    if (fileId) {
        const SymbolMap s = symbols(fileId);
        for (auto it = s.begin(); it != s.end(); ++it) {
            if (!RTags::isReference(it->second.kind) && (symbolName.isEmpty() || matchSymbolName(symbolName, it->second.symbolName)))
                ret.insert(it->first);
        }
    } else if (symbolName.isEmpty()) {
        for (auto it = mSymbols.constBegin(); it != mSymbols.constEnd(); ++it) {
            if (!RTags::isReference(it->second.kind))
                ret.insert(it->first);
        }
    } else {
        auto it = mSymbolNames.lower_bound(symbolName);
        while (it != mSymbolNames.end() && it->first.startsWith(symbolName)) {
            if (matchSymbolName(symbolName, it->first))
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
            node.isDefinition = found->second.isDefinition();
            if (flags & Sort_DeclarationOnly && node.isDefinition) {
                const CursorInfo decl = found->second.bestTarget(mSymbols);
                if (!decl.isNull())
                    continue;
            }
            node.kind = found->second.kind;
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
String Project::dumpJobs() const
{
    String ret;
    const char *types[] = { "Invalid",
                            "Makefile",
                            "Dirty",
                            "Dump",
                            "Remote" };

    for (auto it = mJobs.begin(); it != mJobs.end(); ++it) {
        ret << Location::path(it->first) << it->second.job->source << types[it->second.job->type]
            << it->second.crashCount
            << (it->second.pendingType == IndexerJob::Invalid ? String() : it->second.pendingSource.toString())
            << "\n";
    }
    return ret;
}

void Project::watch(const Path &file)
{
    const Path dir = file.parentDir();
    if (dir.isEmpty()) {
        error() << "Got empty parent dir for" << file;
    } else if (((Server::instance()->options().options & Server::WatchSystemPaths) || !dir.isSystem())
               && mWatchedPaths.insert(dir)) {
        mWatcher.watch(dir);
    }
}

bool Project::hasSource(const Source &source) const
{
    auto it = mSources.find(source.key());
    return it != mSources.end() && it->second == source;
}
