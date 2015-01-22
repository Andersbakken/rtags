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
#include "Diagnostic.h"
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
#include <rct/Value.h>
#include <rct/Rct.h>
#include <rct/ReadLocker.h>
#include <rct/RegExp.h>
#include <rct/Thread.h>
#include <memory>

enum { DirtyTimeout = 100 };

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
    : mPath(path), mState(Unloaded), mJobCounter(0), mJobsStarted(0)
{
    Path srcPath = mPath;
    RTags::encodePath(srcPath);
    const Server::Options &options = Server::instance()->options();
    mProjectFilePath = options.dataDir + srcPath + "/project";


    if (!(options.options & Server::NoFileSystemWatch)) {
        mWatcher.modified().connect(std::bind(&Project::onFileModifiedOrRemoved, this, std::placeholders::_1));
        mWatcher.removed().connect(std::bind(&Project::onFileModifiedOrRemoved, this, std::placeholders::_1));
    }
    if (!(options.options & Server::NoFileManagerWatch)) {
        mWatcher.removed().connect(std::bind(&Project::reloadFileManager, this));
        mWatcher.added().connect(std::bind(&Project::reloadFileManager, this));
    }
    mDirtyTimer.timeout().connect(std::bind(&Project::onDirtyTimeout, this, std::placeholders::_1));
}

Project::~Project()
{
    assert(EventLoop::isMainThread());
    assert(mActiveJobs.isEmpty());
}

void Project::init()
{
    assert(mState == Unloaded);
    mState = Inited;
    fileManager.reset(new FileManager);
    fileManager->init(shared_from_this(), FileManager::Asynchronous);
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
    case Loaded:
        return false;
    }
    mState = Loaded;
    DataFile file(mProjectFilePath);
    if (!file.open(DataFile::Read)) {
        if (!file.error().isEmpty())
            error("Restore error %s: %s", mPath.constData(), file.error().constData());
        Path::rm(mProjectFilePath);
        return false;
    }

    file >> mSources >> mVisitedFiles >> mDependencies;

    for (const auto& dep : mDependencies) {
        watch(Location::path(dep.first));
    }

    bool needsSave = false;
    std::unique_ptr<ComplexDirty> dirty;

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

    if (needsSave)
        save();
    startDirtyJobs(dirty.get());
    return true;
}

void Project::unload()
{
    if (mState == Unloaded)
        return;
    save(); // we always save since sources very likely had their Enabledness changed
    for (const auto &job : mActiveJobs) {
        assert(job.second);
        Server::instance()->jobScheduler()->abort(job.second);
    }

    mActiveJobs.clear();
    fileManager.reset();

    mFiles.clear();
    mSources.clear();
    mVisitedFiles.clear();
    mDependencies.clear();
    mState = Unloaded;
    mDirtyTimer.stop();
}

bool Project::match(const Match &p, bool *indexed) const
{
    Path paths[] = { p.pattern(), p.pattern() };
    paths[1].resolve();
    const int count = paths[1].compare(paths[0]) ? 2 : 1;
    bool ret = false;
    const Path resolvedPath = mPath.resolved();
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
    if (mState != Loaded) {
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
        logDirect(RTags::CompilationErrorXml, Diagnostic::format(indexData->diagnostics));
        if (!(options.options & Server::NoProgress)) {
            log(RTags::CompilationErrorXml,
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<progress index=\"%d\" total=\"%d\"></progress>",
                idx, mJobCounter);
        }
    }

    Set<uint32_t> newFiles;
    int symbolNames = 0;
    addFixIts(indexData->visited, indexData->fixIts);
    removeDependencies(indexData->fileId());
    addDependencies(indexData->dependencies, newFiles);
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

    save();
    if (mActiveJobs.isEmpty()) {
        double timerElapsed = (mTimer.elapsed() / 1000.0);
        const double averageJobTime = timerElapsed / mJobsStarted;
        const String msg = String::format<1024>("Jobs took %.2fs%s. We're using %lldmb of memory. ",
                                                timerElapsed, mJobsStarted > 1 ? String::format(", (avg %.2fs)", averageJobTime).constData() : "",
                                                MemoryMonitor::usage() / (1024 * 1024), symbolNames);
        error() << msg;
        mJobsStarted = mJobCounter = 0;

        // error() << "Finished this
    }
}

bool Project::save()
{
    DataFile file(mProjectFilePath);
    if (!file.open(DataFile::Write)) {
        error("Save error %s: %s", mProjectFilePath.constData(), file.error().constData());
        return false;
    }
    file << mSources << mVisitedFiles << mDependencies;
    if (!file.flush()) {
        error("Save error %s: %s", mProjectFilePath.constData(), file.error().constData());
        return false;
    }

    return true;
}

static inline void markActive(SourceMap::iterator start, uint32_t buildId, const SourceMap::iterator end)
{
    const uint32_t fileId = start->second.fileId;
    while (start != end) {
        uint32_t f, b;
        Source::decodeKey(start->first, f, b);
        if (f != fileId)
            break;

        if (b == buildId) {
            start->second.flags |= Source::Active;
        } else {
            start->second.flags &= ~Source::Active;
        }
        ++start;
    }
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

    const uint64_t key = job->source.key();
    if (Server::instance()->suspended() && mSources.contains(key) && (job->flags & IndexerJob::Compile)) {
        return;
    }

    if (job->flags & IndexerJob::Compile) {
        const auto &options = Server::instance()->options();
        if (options.options & Server::NoFileSystemWatch) {
            auto it = mSources.lower_bound(Source::key(job->source.fileId, 0));
            if (it != mSources.end()) {
                uint32_t f, b;
                Source::decodeKey(it->first, f, b);
                if (f == job->source.fileId) {
                    // When we're not watching the file system, we ignore
                    // updating compiles. This means that you always have to
                    // do check-reindex to build existing files!
                    return;
                }
            }
        } else {
            auto cur = mSources.find(key);
            if (cur != mSources.end()) {
                if (!(cur->second.flags & Source::Active))
                    markActive(mSources.lower_bound(Source::key(job->source.fileId, 0)), cur->second.buildRootId, mSources.end());
                if (cur->second.compareArguments(job->source)) {
                    // no updates
                    return;
                }
            } else {
                auto it = mSources.lower_bound(Source::key(job->source.fileId, 0));
                const auto start = it;
                const bool disallowMultiple = options.options & Server::DisallowMultipleSources;
                bool unsetActive = false;
                while (it != mSources.end()) {
                    uint32_t f, b;
                    Source::decodeKey(it->first, f, b);
                    if (f != job->source.fileId)
                        break;

                    if (it->second.compareArguments(job->source)) {
                        markActive(start, b, mSources.end());
                        // no updates
                        return;
                    } else if (disallowMultiple) {
                        mSources.erase(it++);
                        continue;
                    }
                    unsetActive = true;
                    ++it;
                }
                if (unsetActive) {
                    assert(!disallowMultiple);
                    markActive(start, 0, mSources.end());
                }
            }
        }
    }

    Source &src = mSources[key];
    src = job->source;
    src.flags |= Source::Active;

    std::shared_ptr<IndexerJob> &ref = mActiveJobs[key];
    if (ref) {
        releaseFileIds(ref->visited);
        Server::instance()->jobScheduler()->abort(ref);
        --mJobCounter;
    }
    ref = job;

    ++mJobsStarted;
    if (!mJobCounter++) {
        mTimer.start();
    }

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

void Project::removeDependencies(uint32_t fileId)
{
    mDependencies.remove(fileId);
    for (auto it = mDependencies.begin(); it != mDependencies.end(); ++it) {
        it->second.remove(fileId);
    }
}

void Project::addDependencies(const DependencyMap &deps, Set<uint32_t> &newFiles)
{
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
            removeDependencies(fileId);
            ++count;
            unlink(sourceFilePath(fileId, String()).constData());
        } else {
            ++it;
        }
    }
    return count;
}

int Project::startDirtyJobs(Dirty *dirty, const UnsavedFiles &unsavedFiles)
{
    List<Source> toIndex;
    for (const auto &source : mSources) {
        if (source.second.flags & Source::Active && dirty->isDirty(source.second)) {
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

    return toIndex.size();
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

void Project::addFixIts(const Hash<uint32_t, bool> &visited, const FixItMap &fixIts)
                        {
    for (auto v : visited) {
        if (v.second) {
            const auto fit = fixIts.find(v.first);
            if (fit == fixIts.end()) {
                mFixIts.erase(v.first);
            } else {
                mFixIts[v.first] = fit->second;
            }
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
    auto processCursor = [&ret, &symbolName, this](uint32_t fileId) {
        auto s = openCursors(fileId);
        if (!s)
            return;
        const int count = s->count();
        for (int i=0; i<count; ++i) {
            const Cursor c = s->valueAt(i);
            if (!RTags::isReference(c.kind)
                && (symbolName.isEmpty() || matchSymbolName(symbolName, c.symbolName, checkFunction(c.kind)))) {
                ret.insert(s->keyAt(i));
            }
        }
    };

    if (fileId) {
        processCursor(fileId);
    } else if (symbolName.isEmpty()) {
        for (const auto &dep : mDependencies) {
            processCursor(dep.first);
        }
    } else {
        for (const auto &dep : mDependencies) {
            auto symNames = openSymbolNames(dep.first);
            if (symNames) {
                int idx = symNames->lowerBound(symbolName);
                const int count = symNames->count();
                while (idx < count) {
                    const String s = symNames->keyAt(idx);
                    if (!s.startsWith(symbolName))
                        break;
                    if (matchSymbolName(symbolName, s, true)) // assume function
                        ret.unite(symNames->valueAt(idx));
                    ++idx;
                }
            }
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
        const Cursor cursor = findCursor(*it);
        if (!cursor.isNull()) {
            node.isDefinition = cursor.isDefinition();
            if (flags & Sort_DeclarationOnly && node.isDefinition) {
                const Cursor decl = findCursor(findTarget(cursor));
                if (!decl.isNull() && !decl.isDefinition()) {
                    assert(decl.usr == cursor.usr);
                    continue;
                }
            }
            node.kind = cursor.kind;
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

String Project::toCompilationDatabase() const
{
    const unsigned int flags = (Source::IncludeCompiler | Source::IncludeSourceFile | Source::IncludeDefines
                                | Source::IncludeIncludepaths | Source::QuoteDefines | Source::FilterBlacklist);
    Value ret(List<Value>(mSources.size()));
    int i = 0;
    for (const auto &source : mSources) {
        Value unit;
        unit["directory"] = source.second.directory;
        unit["file"] = source.second.sourceFile();
        unit["command"] = source.second.toCommandLine(flags);
        ret[i++] = unit;
    }

    return ret.toJSON(true);
}
Path Project::sourceFilePath(uint32_t fileId, const String &type) const
{
    return RTags::encodeSourceFilePath(Server::instance()->options().dataDir, mPath, fileId) + type;
}

Cursor Project::findCursor(const Location &location, int *index) const
{
    if (index)
        *index = -1;
    if (location.isNull())
        return Cursor();
    auto cursors = openCursors(location.fileId());
    if (!cursors)
        return Cursor();

    bool exact = false;
    int idx = cursors->lowerBound(location, &exact);
    if (exact) {
        if (index)
            *index = idx;
        return cursors->valueAt(idx);
    }
    switch (idx) {
    case 0:
        return Cursor();
    case -1:
        idx = cursors->count() - 1;
        break;
    default:
        --idx;
        break;
    }

    const Cursor &ret = cursors->valueAt(idx);
    if (ret.location.fileId() != location.fileId()
        || ret.location.line() != location.line()
        || (location.column() - ret.location.column() >= ret.symbolLength)) {
        return Cursor();
    }
    if (index)
        *index = idx;
    return ret;
}

Location Project::findTarget(const Cursor &cursor) const
{
    if (cursor.isNull())
        return Location();
    if (cursor.isClass() && cursor.isDefinition())
        return Location();

    Location targetLocation;
    switch (cursor.kind) {
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_StructDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
    case CXCursor_FunctionTemplate: {
        Set<uint32_t> files;
        if (cursor.isDefinition()) {
            files = dependencies(cursor.location.fileId(), ArgDependsOn);
        } else {
            files = dependencies(cursor.location.fileId(), DependsOnArg);
        }
        // error() << files << cursor.location;
        const Set<Cursor> cursors = findByUsr(files, cursor.usr);
        for (const auto &c : cursors) {
            if (cursor.isDefinition() != c.isDefinition()) {
                targetLocation = c.location;
                break;
            }
        }
        break; }
    default:
        const auto targetsDb = openTargets(cursor.location.fileId());
        if (!targetsDb)
            return Location();
        targetLocation = RTags::bestTarget(targetsDb->value(cursor.location));
        break;
    }

    return targetLocation;
}

Set<Cursor> Project::findByUsr(const Set<uint32_t> &files, const String &usr) const
{
    Set<Cursor> ret;
    for (uint32_t fileId : files) {
        auto usrs = openUsrs(fileId);
        // error() << usrs << Location::path(fileId);
        if (usrs) {
            for (const Location &loc : usrs->value(usr)) {
                const Cursor c = findCursor(loc);
                if (!c.isNull())
                    ret.insert(c);
            }
        }
    }
    return ret;
}


