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
        auto s = openSymbols(fileId);
        if (!s)
            return;
        const int count = s->count();
        for (int i=0; i<count; ++i) {
            const Symbol c = s->valueAt(i);
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

List<RTags::SortedSymbol> Project::sort(const Set<Location> &locations, unsigned int flags) const
{
    List<RTags::SortedSymbol> sorted;
    sorted.reserve(locations.size());
    for (auto it = locations.begin(); it != locations.end(); ++it) {
        RTags::SortedSymbol node(*it);
        const Symbol symbol = findSymbol(*it);
        if (!symbol.isNull()) {
            node.isDefinition = symbol.isDefinition();
            if (flags & Sort_DeclarationOnly && node.isDefinition) {
                const Symbol decl = findSymbol(findTarget(symbol));
                if (!decl.isNull() && !decl.isDefinition()) {
                    assert(decl.usr == symbol.usr);
                    continue;
                }
            }
            node.kind = symbol.kind;
        }
        sorted.push_back(node);
    }

    if (flags & Sort_Reverse) {
        std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedSymbol>());
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

Symbol Project::findSymbol(const Location &location, int *index) const
{
    if (index)
        *index = -1;
    if (location.isNull())
        return Symbol();
    auto symbols = openSymbols(location.fileId());
    if (!symbols)
        return Symbol();

    bool exact = false;
    int idx = symbols->lowerBound(location, &exact);
    if (exact) {
        if (index)
            *index = idx;
        return symbols->valueAt(idx);
    }
    switch (idx) {
    case 0:
        return Symbol();
    case -1:
        idx = symbols->count() - 1;
        break;
    default:
        --idx;
        break;
    }

    const Symbol &ret = symbols->valueAt(idx);
    if (ret.location.fileId() != location.fileId()
        || ret.location.line() != location.line()
        || (location.column() - ret.location.column() >= ret.symbolLength)) {
        return Symbol();
    }
    if (index)
        *index = idx;
    return ret;
}

Map<Location, uint16_t> Project::findTargets(const Symbol &symbol) const
{
    Map<Location, uint16_t> ret;
    if (symbol.isNull())
        return ret;
    if (symbol.isClass() && symbol.isDefinition())
        return ret;

    switch (symbol.kind) {
    case CXCursor_ClassDecl:
    case CXCursor_ClassTemplate:
    case CXCursor_StructDecl:
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
    case CXCursor_FunctionTemplate: {
        Set<uint32_t> files;
        if (symbol.isDefinition()) {
            files = dependencies(symbol.location.fileId(), ArgDependsOn);
        } else {
            files = dependencies(symbol.location.fileId(), DependsOnArg);
        }
        // error() << files << symbol.location;
        const Set<Symbol> symbols = findByUsr(files, symbol.usr);
        for (const auto &c : symbols) {
            if (symbol.isDefinition() != c.isDefinition()) {
                ret.insert(c.location, c.targetsValue());
                break;
            }
        }
        break; }
    default:
        if (const auto targetsDb = openTargets(symbol.location.fileId()))
            ret = targetsDb->value(symbol.location);
        break;
    }

    return ret;
}

Set<Symbol> Project::findByUsr(const Set<uint32_t> &files, const String &usr) const
{
    Set<Symbol> ret;
    for (uint32_t fileId : files) {
        auto usrs = openUsrs(fileId);
        // error() << usrs << Location::path(fileId);
        if (usrs) {
            for (const Location &loc : usrs->value(usr)) {
                const Symbol c = findSymbol(loc);
                if (!c.isNull())
                    ret.insert(c);
            }
        }
    }
    return ret;
}

#if 0
Set<Symbol> CursorInfo::referenceInfos(const Set<Symbol> &map) const
{
    Set<Symbol> ret;
    for (auto it = references.begin(); it != references.end(); ++it) {
        auto found = RTags::findSymbolInfo(map, *it);
        if (found != map.end()) {
            ret[*it] = found->second;
        }
    }
    return ret;
}
#endif

template <typename Key, typename Value>
class FileMapCache
{
public:
    FileMapCache(const Project *p, const String &t)
        : project(p), type(t)
    {}

    std::shared_ptr<FileMap<Key, Value> > open(uint32_t fileId)
    {
        if (cache.contains(fileId))
            return cache[fileId];
        auto ret = project->openFileMap<Key, Value>(fileId, type);
        cache[fileId] = ret;
        return ret;
    }

    Map<uint32_t, std::shared_ptr<FileMap<Key, Value> > > cache;

    const Project *project;
    const String type;
};

Set<Symbol> Project::findCallers(const Symbol &symbol) const
{
    if (symbol.isNull())
        return Set<Symbol>();

    Set<Symbol> inputs;
    inputs.insert(symbol);
    if (symbol.isClass()) {
        inputs.unite(findByUsr(dependencies(symbol.location.fileId(), ArgDependsOn), symbol.usr));
    } else if (!symbol.isReference()) {
        const Symbol target = findSymbol(findTarget(symbol));
        if (!target.isNull())
            inputs.insert(target);
        inputs.unite(findVirtuals(symbol));
    }
    FileMapCache<Location, Symbol> symbolsCache(this, fileMapName(Symbols));
    FileMapCache<Location, Map<Location, uint16_t> > targetsCache(this, fileMapName(Targets));
    const bool isClazz = symbol.isClass();
    Set<Symbol> ret;
    for (const Symbol &input : inputs) {
        for (const auto &dep : dependencies(input.location.fileId(), DependsOnArg)) {
            auto targets = targetsCache.open(dep);
            if (!targets)
                continue;
            const int count = targets->count();
            for (int i=0; i<count; ++i) {
                for (const std::pair<Location, uint16_t> &reference : targets->valueAt(i)) {
                    error() << "Comparing" << reference.first << "with" << input.location;
                    const CXCursorKind refKind = RTags::targetsValueKind(reference.second);
                    if (reference.first == input.location) {
                        if (isClazz && refKind == CXCursor_CallExpr) {
                            printf("[%s:%d]: if (isClazz && reference.second == CXCursor_CallExpr) {\n", __FILE__, __LINE__); fflush(stdout);
                            continue;
                        }
                        if (RTags::isReference(refKind)
                            || (symbol.kind == CXCursor_Constructor
                                && (refKind == CXCursor_VarDecl || refKind == CXCursor_FieldDecl))) {
                            const Symbol sym = findSymbol(reference.first);
                            if (!sym.isNull())
                                ret.insert(sym);
                        } else if (!RTags::isReference(refKind)) {
                            printf("[%s:%d]: } else if (!RTags::isReference(reference.second)) {\n", __FILE__, __LINE__); fflush(stdout);
                        } else {
                            printf("[%s:%d]: \n", __FILE__, __LINE__); fflush(stdout);
                        }
                    }
                }
            }
        }
    }
    return ret;
}

#if 0
enum Mode {
    ClassRefs,
    VirtualRefs,
    NormalRefs
};

static inline void allImpl(const Set<Symbol> &map, const Location &loc, const std::shared_ptr<CursorInfo> &info, Set<Symbol> &out, Mode mode, unsigned kind)
{
    if (out.contains(loc))
        return;
    out[loc] = info;
    const Set<Symbol> targets = info->targetInfos(map);
    for (auto t = targets.begin(); t != targets.end(); ++t) {
        bool ok = false;
        switch (mode) {
        case VirtualRefs:
        case NormalRefs:
            ok = (t->second->kind == kind);
            break;
        case ClassRefs:
            ok = (t->second->isClass() || t->second->kind == CXCursor_Destructor || t->second->kind == CXCursor_Constructor);
            break;
        }
        if (ok)
            allImpl(map, t->first, t->second, out, mode, kind);
    }
    const Set<Symbol> refs = info->referenceInfos(map);
    for (auto r = refs.begin(); r != refs.end(); ++r) {
        switch (mode) {
        case NormalRefs:
            out[r->first] = r->second;
            break;
        case VirtualRefs:
            if (r->second->kind == kind) {
                allImpl(map, r->first, r->second, out, mode, kind);
            } else {
                out[r->first] = r->second;
            }
            break;
        case ClassRefs:
            if (info->isClass()) // for class/struct we want the references inserted directly regardless and also recursed
                out[r->first] = r->second;
            if (r->second->isClass()
                || r->second->kind == CXCursor_Destructor
                || r->second->kind == CXCursor_Constructor) { // if is a constructor/destructor/class reference we want to recurse it
                allImpl(map, r->first, r->second, out, mode, kind);
            }
        }
    }
}
#endif

static void addReferences(const Symbol &symbol, Map<Location, Symbol> &symbols,
                          const Set<uint32_t> &files,
                          const std::shared_ptr<Project> &project)
{
    Map<uint32_t, std::shared_ptr<FileMap<Location, Map<Location, uint16_t> > > > seen;
    auto openDB = [&seen, &project](uint32_t fileId) {
        auto it = seen.find(fileId);
        if (it != seen.end())
            return it->second;
        auto db = project->openTargets(fileId);
        seen[fileId] = db;
        return db;
    };
    for (uint32_t file : files) {
        auto targets = openDB(file);
        if (!targets)
            continue;
        const int count = targets->count();
        for (int i=0; i<count; ++i) {
            for (const auto &target : targets->valueAt(i)) {
                if (target.first == symbol.location) {
                    if (symbols.contains(target.first)) {
                        auto c = project->findSymbol(target.first);
                        if (!c.isNull()) {
                            symbols[target.first] = c;
                            if (!c.isReference()) {
                                addReferences(c, symbols, project->dependencies(target.first.fileId(), Project::DependsOnArg), project);
                            }
                        }
                    }
                    break;
                }
            }
        }
    }
}

Set<Symbol> Project::findAllReferences(const Symbol &loc) const
{
    // Set<Symbol> ret;
    // Mode mode = NormalRefs;
    // switch (kind) {
    // case CXCursor_Constructor:
    // case CXCursor_Destructor:
    //     mode = ClassRefs;
    //     break;
    // case CXCursor_CXXMethod:
    //     mode = VirtualRefs;
    //     break;
    // default:
    //     mode = isClass() ? ClassRefs : VirtualRefs;
    //     break;
    // }

    // allImpl(map, loc, copy(), ret, mode, kind);
    Set<Symbol> ret;
    return ret;
}
#if 0
#endif

Set<Symbol> Project::findVirtuals(const Symbol &symbol) const
{
    // Set<Symbol> ret;
    // ret[loc] = copy();
    // const Set<Symbol> s = (kind == CXCursor_CXXMethod ? allReferences(loc, map) : targetInfos(map));
    // for (auto it = s.begin(); it != s.end(); ++it) {
    //     if (it->second->kind == kind)
    //         ret[it->first] = it->second;
    // }
    // return ret;
    return Set<Symbol>();
}


// Set<Location> Project::findReferences(const Symbol &symbol, uint32_t queryFlags) const
// {
//     if (queryFlags & QueryMessage::AllReferences) {

//     } else if (queryFlags & QueryMessage::FindVirtuals) {

//     } else {


//     }
// }
