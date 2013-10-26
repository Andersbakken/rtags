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
#include "IndexerJobClang.h"
#include "RTags.h"
#include "ReparseJob.h"
#include "Server.h"
#include "Server.h"
// #include "ValidateDBJob.h"
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
            project->restore();
            EventLoop::mainEventLoop()->callLater(std::bind(&Project::startPendingJobs, project.get()));
        }
    }
private:
    std::weak_ptr<Project> mProject;
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
    std::lock_guard<std::mutex> lock(mMutex);
    assert(mState == Unloaded);
    mState = Inited;
    fileManager.reset(new FileManager);
    fileManager->init(shared_from_this(), FileManager::Asynchronous);
}

bool Project::restore()
{
    bool needsSave = false;
    assert(state() == Loading);
    StopWatch timer;
    Path path = mPath;
    RTags::encodePath(path);
    const Path p = Server::instance()->options().dataDir + path;
    bool restoreError = false;
    FILE *f = fopen(p.constData(), "r");
    if (!f) {
        return false;
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
                  p.constData(), mPath.constData());
            restoreError = true;
            goto end;
        }
    }
    {

        in >> mSymbols >> mSymbolNames >> mUsr >> mDependencies >> mSources >> mVisitedFiles;

        DependencyMap reversedDependencies;
        Set<uint32_t> dirty;
        // these dependencies are in the form of:
        // Path.cpp: Path.h, String.h ...
        // mDependencies are like this:
        // Path.h: Path.cpp, Server.cpp ...

        {
            DependencyMap::iterator it = mDependencies.begin();
            while (it != mDependencies.end()) {
                const Path file = Location::path(it->first);
                if (!file.exists()) {
                    error() << "File doesn't exist" << file;
                    mDependencies.erase(it++);
                    needsSave = true;
                    continue;
                }
                watch(file);
                for (Set<uint32_t>::const_iterator s = it->second.begin(); s != it->second.end(); ++s)
                    reversedDependencies[*s].insert(it->first);
                ++it;
            }
        }

        SourceMap::iterator it = mSources.begin();
        while (it != mSources.end()) {
            if (!it->second.sourceFile().isFile()) {
                error() << it->second.sourceFile() << "seems to have disappeared";
                dirty.insert(it->first);
                mSources.erase(it++);
                needsSave = true;
            } else {
                const time_t parsed = it->second.parsed;
                // error() << "parsed" << String::formatTime(parsed, String::DateTime) << parsed << it->second.sourceFile;
                if (mDependencies.value(it->first).contains(it->first)) {
                    assert(mDependencies.value(it->first).contains(it->first));
                    assert(mDependencies.contains(it->first));
                    const Set<uint32_t> &deps = reversedDependencies[it->first];
                    for (Set<uint32_t>::const_iterator d = deps.begin(); d != deps.end(); ++d) {
                        if (!dirty.contains(*d) && Location::path(*d).lastModified() > parsed) {
                            // error() << Location::path(*d).lastModified() << "is more than" << parsed;
                            dirty.insert(*d);
                        }
                    }
                }
                ++it;
            }
        }
        if (!dirty.isEmpty()) {
            startDirtyJobs(dirty);
        } else if (needsSave) {
            save();
        }
    }
end:
    // fileManager->jsFilesChanged().connect(this, &Project::onJSFilesAdded);
    // onJSFilesAdded();
    fclose(f);

    if (restoreError) {
        Path::rm(p);
    } else {
        error() << "Restored project" << mPath << "in" << timer.elapsed() << "ms";
    }

    return !restoreError;
}

void Project::startPendingJobs() // lock always held
{
    Hash<uint32_t, JobData> pendingJobs;
    {
        std::lock_guard<std::mutex> lock(mMutex);
        mState = Loaded;
        pendingJobs = std::move(mJobs);
    }
    for (Hash<uint32_t, JobData>::const_iterator it = pendingJobs.begin(); it != pendingJobs.end(); ++it) {
        assert(!it->second.pending.isNull());
        assert(it->second.pendingType != IndexerJob::Invalid);
        index(it->second.pending, it->second.pendingType);
    }
}

Project::State Project::state() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mState;
}

void Project::load(FileManagerMode mode)
{
    {
        std::lock_guard<std::mutex> lock(mMutex);
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
    }
    RestoreThread *thread = new RestoreThread(shared_from_this());
    thread->start();
}

void Project::unload()
{
    std::lock_guard<std::mutex> lock(mMutex);
    for (Hash<uint32_t, JobData>::const_iterator it = mJobs.begin(); it != mJobs.end(); ++it) {
        if (it->second.job)
            it->second.job->abort();
    }
    mJobs.clear();
    fileManager.reset();

    mSymbols.clear();
    mErrorSymbols.clear();
    mSymbolNames.clear();
    mUsr.clear();
    mFiles.clear();
    mSources.clear();
    mVisitedFiles.clear();
    mDependencies.clear();

    for (LinkedList<CachedUnit*>::const_iterator it = mCachedUnits.begin(); it != mCachedUnits.end(); ++it) {
        delete *it;
    }
    mCachedUnits.clear();
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
    IndexerJob::IndexType pendingType = IndexerJob::Invalid;
    bool syncNow = false;
    {
        std::lock_guard<std::mutex> lock(mMutex);
        const Hash<uint32_t, JobData>::iterator it = mJobs.find(indexData->fileId);
        if (it == mJobs.end()) {
            error() << "Couldn't find JobData for" << Location::path(indexData->fileId);
            // not sure if this can happen when unloading while jobs are running
            return;
        }

        JobData *jobData = &it->second;
        assert(jobData->job);
        const bool success = !indexData->aborted && !jobData->job->isAborted();
        if (indexData->aborted) {
            ++jobData->crashCount;
        } else {
            jobData->crashCount = 0;
        }

        enum { MaxCrashCount = 5 }; // ### configurable?
        if (jobData->crashCount < MaxCrashCount) {
            if (jobData->pendingType != IndexerJob::Invalid) {
                assert(jobData->job->isAborted());
                std::swap(pendingType, jobData->pendingType);
                std::swap(pending, jobData->pending);
            } else if (!success) {
                pending = jobData->job->source;
                pendingType = jobData->job->type;
                if (!jobData->job->isAborted()) {
                    // ### we should maybe wait a little before restarting or something.
                    error("%s crashed, restarting", jobData->job->source.sourceFile().constData());
                }
            }
        }
        if (pendingType == IndexerJob::Invalid) {
            jobData = 0;
            mJobs.erase(it);

            const int idx = mJobCounter - mJobs.size();
            if (testLog(RTags::CompilationErrorXml)) {
                log(RTags::CompilationErrorXml,
                    "<?xml version=\"1.0\" encoding=\"utf-8\"?><progress index=\"%d\" total=\"%d\"></progress>",
                    idx, mJobCounter);
                logDirect(RTags::CompilationErrorXml, indexData->xmlDiagnostics);
            }
            if (success) {
                mPendingData[indexData->fileId] = indexData;
                mSources[indexData->fileId].parsed = indexData->parseTime;
                error("[%3d%%] %d/%d %s %s.",
                      static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                      String::formatTime(time(0), String::Time).constData(),
                      indexData->message.constData());
            } else {
                assert(indexData->aborted);
                error("[%3d%%] %d/%d %s %s indexing crashed.",
                      static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                      String::formatTime(time(0), String::Time).constData(),
                      Location::path(indexData->fileId).toTilde().constData());
            }
            const int syncThreshold = Server::instance()->options().syncThreshold;
            if (mJobs.isEmpty()) {
                mSyncTimer.restart(indexData->type == IndexerJob::Dirty ? 0 : SyncTimeout, Timer::SingleShot);
            } else if (syncThreshold && mPendingData.size() >= syncThreshold) {
                syncNow = true;
            }
        } else {
            jobData->job.reset();
        }
    }
    if (syncNow)
        sync();
    if (pendingType != IndexerJob::Invalid) {
        assert(!pending.isNull());
        index(pending, pendingType);
        --mJobCounter;
    }
}

bool Project::save()
{
    std::lock_guard<std::mutex> lock(mMutex);
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

// void Project::dump(const Path &path, const QueryMessage &Connection *conn)
// {
//     const uint32_t fileId = Location::fileId(fileId);
//     Source source = source(fileId);
//     if (source.isNull()) {
//         conn->write<64>("No source information for %s", path.constData());
//         conn->finish();;
//         return;
//     }

//     std::lock_guard<std::mutex> lock(mMutex);
//     assert(conn);
// }

void Project::index(const Source &source, IndexerJob::IndexType type)
{
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(source.sourceFile().constData(), fileFilter))
        return;

    std::lock_guard<std::mutex> lock(mMutex);
    JobData &data = mJobs[source.fileId];
    if (mState != Loaded) {
        // error() << "Index called at" << static_cast<int>(mState) << "time. Setting pending" << source.sourceFile();
        data.pending = source;
        data.pendingType = IndexerJob::Makefile;
        return;
    }
    if (data.job) {
        // error() << "There's already something here for" << source.sourceFile();
        if (data.job->abort()) {
            // error() << "Aborting and setting pending" << source.sourceFile();
            data.pending = source;
            data.pendingType = type;
        } else {
            // error() << "Not started yet, updating" << source.sourceFile();
            // not started yet
            data.job->source = source;
            data.job->type = type;
        }
        return;
    }
    std::shared_ptr<Project> project = shared_from_this();

    mSources[source.fileId] = source;
    watch(source.sourceFile());

    data.pending.clear();
    data.pendingType = IndexerJob::Invalid;
    mPendingData.remove(source.fileId);

    if (!mJobCounter++)
        mTimer.start();

    data.job = Server::instance()->factory().createJob(type, project, source);
    if (!data.job) {
        error() << "Failed to create job for" << source;
        mJobs.erase(source.fileId);
        return;
    }
    ++mNextId;
    mSyncTimer.stop();
    data.job->start();
}

bool Project::index(const Source &s)
{
    assert(!s.isNull());

    const Source current = source(s.fileId);
    if (current.compare(s)) {
        debug() << s.sourceFile() << " is not dirty. ignoring";
        return false;
    }

    index(s, IndexerJob::Makefile);
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

SourceMap Project::sources() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mSources;
}

Source Project::source(uint32_t fileId) const
{
    if (fileId) {
        std::lock_guard<std::mutex> lock(mMutex);
        return mSources.value(fileId);
    }
    return Source();
}

void Project::addDependencies(const DependencyMap &deps, Set<uint32_t> &newFiles)
{
    StopWatch timer;

    const DependencyMap::const_iterator end = deps.end();
    for (DependencyMap::const_iterator it = deps.begin(); it != end; ++it) {
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
    std::lock_guard<std::mutex> lock(mMutex);
    if (mode == DependsOnArg)
        return mDependencies.value(fileId);

    Set<uint32_t> ret;
    const DependencyMap::const_iterator end = mDependencies.end();
    for (DependencyMap::const_iterator it = mDependencies.begin(); it != end; ++it) {
        if (it->second.contains(fileId))
            ret.insert(it->first);
    }
    return ret;
}

int Project::reindex(const Match &match)
{
    Set<uint32_t> dirty;
    {
        std::lock_guard<std::mutex> lock(mMutex);

        const DependencyMap::const_iterator end = mDependencies.end();
        for (DependencyMap::const_iterator it = mDependencies.begin(); it != end; ++it) {
            if (match.isEmpty() || match.match(Location::path(it->first))) {
                dirty.insert(it->first);
            }
        }
        if (dirty.isEmpty())
            return 0;
    }
    startDirtyJobs(dirty);
    return dirty.size();
}

int Project::remove(const Match &match)
{
    int count = 0;
    Set<uint32_t> dirty;
    {
        std::lock_guard<std::mutex> lock(mMutex);
        SourceMap::iterator it = mSources.begin();
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
    }
    if (count)
        startDirtyJobs(dirty);
    return count;
}


void Project::onValidateDBJobErrors(const Set<Location> &errors)
{
    std::lock_guard<std::mutex> lock(mMutex);
    mPreviousErrors = errors;
}

void Project::startDirtyJobs(const Set<uint32_t> &dirty)
{
    Set<uint32_t> dirtyFiles;
    {
        std::lock_guard<std::mutex> lock(mMutex);
        for (Set<uint32_t>::const_iterator it = dirty.begin(); it != dirty.end(); ++it) {
            const Set<uint32_t> deps = mDependencies.value(*it);
            dirtyFiles.insert(*it);
            if (!deps.isEmpty())
                dirtyFiles += deps;
        }
        mVisitedFiles -= dirtyFiles;
    }

    bool indexed = false;
    for (Set<uint32_t>::const_iterator it = dirtyFiles.begin(); it != dirtyFiles.end(); ++it) {
        const SourceMap::const_iterator found = mSources.find(*it);
        if (found != mSources.end()) {
            index(found->second, IndexerJob::Dirty);
            indexed = true;
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
    SymbolNameMap::const_iterator it = symbolNames.begin();
    const SymbolNameMap::const_iterator end = symbolNames.end();
    while (it != end) {
        Set<Location> &value = current[it->first];
        value.unite(it->second);
        ++it;
    }
}

static inline void joinCursors(SymbolMap &symbols, const Set<Location> &locations)
{
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        SymbolMap::iterator c = symbols.find(*it);
        if (c != symbols.end()) {
            CursorInfo &cursorInfo = c->second;
            for (Set<Location>::const_iterator innerIt = locations.begin(); innerIt != locations.end(); ++innerIt) {
                if (innerIt != it)
                    cursorInfo.targets.insert(*innerIt);
            }
            // ### this is filthy, we could likely think of something better
        }
    }
}

static inline void writeUsr(const UsrMap &usr, UsrMap &current, SymbolMap &symbols)
{
    UsrMap::const_iterator it = usr.begin();
    const UsrMap::const_iterator end = usr.end();
    while (it != end) {
        Set<Location> &value = current[it->first];
        int count = 0;
        value.unite(it->second, &count);
        if (count && value.size() > 1)
            joinCursors(symbols, value);
        ++it;
    }
}

static inline void writeErrorSymbols(const SymbolMap &symbols, ErrorSymbolMap &errorSymbols, const Hash<uint32_t, int> &errors)
{
    for (Hash<uint32_t, int>::const_iterator it = errors.begin(); it != errors.end(); ++it) {
        if (it->second) {
            SymbolMap &symbolsForFile = errorSymbols[it->first];
            if (symbolsForFile.isEmpty()) {
                const Location loc(it->first, 1, 0);
                SymbolMap::const_iterator sit = symbols.lower_bound(loc);
                while (sit != symbols.end() && sit->first.fileId() == it->first) {
                    symbolsForFile[sit->first] = sit->second;
                    ++sit;
                }
            }
        } else {
            errorSymbols.remove(it->first);
        }
    }
}

static inline void writeSymbols(SymbolMap &symbols, SymbolMap &current)
{
    if (!symbols.isEmpty()) {
        if (current.isEmpty()) {
            current = symbols;
        } else {
            SymbolMap::iterator it = symbols.begin();
            const SymbolMap::iterator end = symbols.end();
            while (it != end) {
                SymbolMap::iterator cur = current.find(it->first);
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
    const ReferenceMap::const_iterator end = references.end();
    for (ReferenceMap::const_iterator it = references.begin(); it != end; ++it) {
        const Set<Location> &refs = it->second;
        for (Set<Location>::const_iterator rit = refs.begin(); rit != refs.end(); ++rit) {
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
    // for (Hash<uint32_t, std::shared_ptr<IndexData> >::iterator it = mPendingData.begin(); it != mPendingData.end(); ++it) {
    //     writeErrorSymbols(mSymbols, mErrorSymbols, it->second->errors);
    // }

    if (!mPendingDirtyFiles.isEmpty()) {
        RTags::dirtySymbols(mSymbols, mPendingDirtyFiles);
        RTags::dirtySymbolNames(mSymbolNames, mPendingDirtyFiles);
        RTags::dirtyUsr(mUsr, mPendingDirtyFiles);
        mPendingDirtyFiles.clear();
    }
    *dirty = sw.restart();

    Set<uint32_t> newFiles;
    for (Hash<uint32_t, std::shared_ptr<IndexData> >::iterator it = mPendingData.begin(); it != mPendingData.end(); ++it) {
        const std::shared_ptr<IndexData> &data = it->second;
        addDependencies(data->dependencies, newFiles);
        addFixIts(data->dependencies, data->fixIts);
        writeSymbols(data->symbols, mSymbols);
        writeUsr(data->usrMap, mUsr, mSymbols);
        writeReferences(data->references, mSymbols);
        writeSymbolNames(data->symbolNames, mSymbolNames);
    }
    for (Set<uint32_t>::const_iterator it = newFiles.begin(); it != newFiles.end(); ++it) {
        watch(Location::path(*it));
    }
    mPendingData.clear();
#warning not done
    // if (Server::instance()->options().options & Server::Validate) {
    //     std::shared_ptr<ValidateDBJob> validate(new ValidateDBJob(shared_from_this(), mPreviousErrors));
    //     Server::instance()->threadPool()->start(std::shared_ptr<WrapperJob>(new WrapperJob(0, validate)));
    // }
    *sync = sw.elapsed();
}

bool Project::isIndexed(uint32_t fileId) const
{
    return mVisitedFiles.contains(fileId) || mSources.contains(fileId);
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

DependencyMap Project::dependencies() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mDependencies;
}

void Project::addCachedUnit(const Path &path, const List<String> &args, CXTranslationUnit unit, int parseCount) // lock always held
{
    assert(unit);
    const int maxCacheSize = Server::instance()->options().completionCacheSize;
    if (!maxCacheSize) {
        clang_disposeTranslationUnit(unit);
        return;
    }
    CachedUnit *cachedUnit = new CachedUnit;
    cachedUnit->path = path;
    cachedUnit->unit = unit;
    cachedUnit->arguments = args;
    cachedUnit->parseCount = parseCount;
    mCachedUnits.push_back(cachedUnit);
    while (mCachedUnits.size() > maxCacheSize) {
        CachedUnit *unit = *mCachedUnits.begin();
        delete unit;
        mCachedUnits.erase(mCachedUnits.begin());
    }
}

LinkedList<CachedUnit*>::iterator Project::findCachedUnit(const Path &path, const List<String> &args)
{
    for (LinkedList<CachedUnit*>::iterator it = mCachedUnits.begin(); it != mCachedUnits.end(); ++it) {
        if ((*it)->path == path && (args.isEmpty() || args == (*it)->arguments))
            return it;
    }
    return mCachedUnits.end();
}

bool Project::initJobFromCache(const Path &path, const List<String> &args,
                               CXTranslationUnit &unit, List<String> *argsOut,
                               int *parseCount)
{
    LinkedList<CachedUnit*>::iterator it = findCachedUnit(path, args);
    if (it != mCachedUnits.end()) {
        CachedUnit *cachedUnit = *it;
        unit = cachedUnit->unit;
        cachedUnit->unit = 0;
        if (argsOut)
            *argsOut = cachedUnit->arguments;
        mCachedUnits.erase(it);
        if (parseCount)
            *parseCount = cachedUnit->parseCount;
        delete cachedUnit;
        return true;
    }
    unit = 0;
    if (parseCount)
        *parseCount = -1;
    return false;
}

bool Project::fetchFromCache(const Path &path, List<String> &args, CXTranslationUnit &unit, int *parseCount)
{
    std::lock_guard<std::mutex> lock(mMutex);
    return initJobFromCache(path, List<String>(), unit, &args, parseCount);
}

void Project::addToCache(const Path &path, const List<String> &args, CXTranslationUnit unit, int parseCount)
{
    std::lock_guard<std::mutex> lock(mMutex);
    addCachedUnit(path, args, unit, parseCount);
}

void Project::addFixIts(const DependencyMap &visited, const FixItMap &fixIts) // lock always held
{
    for (DependencyMap::const_iterator it = visited.begin(); it != visited.end(); ++it) {
        const FixItMap::const_iterator fit = fixIts.find(it->first);
        if (fit == fixIts.end()) {
            mFixIts.erase(it->first);
        } else {
            mFixIts[it->first] = fit->second;
        }
    }
}

String Project::fixIts(uint32_t fileId) const
{
    std::lock_guard<std::mutex> lock(mMutex);
    const FixItMap::const_iterator it = mFixIts.find(fileId);
    String out;
    if (it != mFixIts.end()) {
        const Set<FixIt> &fixIts = it->second;
        if (!fixIts.isEmpty()) {
            Set<FixIt>::const_iterator f = fixIts.end();
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
}

void Project::onJSFilesAdded()
{
    const Set<Path> jsFiles = fileManager->jsFiles();
    for (Set<Path>::const_iterator it = jsFiles.begin(); it != jsFiles.end(); ++it) {
        Source source;
        source.language = Source::JavaScript;
        source.fileId = Location::insertFile(*it);
        index(source);
    }
}

void Project::reloadFileManager()
{
    fileManager->reload(FileManager::Asynchronous);
}

List<std::pair<Path, List<String> > > Project::cachedUnits() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    List<std::pair<Path, List<String> > > ret;

    for (LinkedList<CachedUnit*>::const_iterator it = mCachedUnits.begin(); it != mCachedUnits.end(); ++it)
        ret.append(std::make_pair((*it)->path, (*it)->arguments));
    return ret;
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
        for (SymbolMap::const_iterator it = s.begin(); it != s.end(); ++it) {
            if (!RTags::isReference(it->second.kind) && (symbolName.isEmpty() || matchSymbolName(symbolName, it->second.symbolName)))
                ret.insert(it->first);
        }
    } else if (symbolName.isEmpty()) {
        for (SymbolMap::const_iterator it = mSymbols.begin(); it != mSymbols.end(); ++it) {
            if (!RTags::isReference(it->second.kind))
                ret.insert(it->first);
        }
    } else {
        SymbolNameMap::const_iterator it = mSymbolNames.lower_bound(symbolName);
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
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        RTags::SortedCursor node(*it);
        const SymbolMap::const_iterator found = mSymbols.find(*it);
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
        for (SymbolMap::const_iterator it = mSymbols.lower_bound(Location(fileId, 1, 0));
             it != mSymbols.end() && it->first.fileId() == fileId; ++it) {
            ret[it->first] = it->second;
        }
    }
    return ret;
}
String Project::dumpJobs() const
{
    String ret;
    std::lock_guard<std::mutex> lock(mMutex);
    for (Hash<uint32_t, JobData>::const_iterator it = mJobs.begin(); it != mJobs.end(); ++it) {
        ret << Location::path(it->first) << it->second.job.get() << it->second.crashCount
            << it->second.pending << static_cast<int>(it->second.pendingType) << "\n";
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
