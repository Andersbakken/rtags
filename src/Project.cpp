#include "Project.h"
#include "FileManager.h"
#include "IndexerJob.h"
#include <rct/Rct.h>
#include <rct/Log.h>
#include <rct/MemoryMonitor.h>
#include <rct/Path.h>
#include <rct/Thread.h>
#include "RTags.h"
#include <rct/ReadLocker.h>
#include <rct/RegExp.h>
#include "Server.h"
#include "Server.h"
#include "ValidateDBJob.h"
#include "IndexerJobClang.h"
#include "ReparseJob.h"
#include <math.h>

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
        if (std::shared_ptr<Project> project = mProject.lock()) {
            project->restore();
            project->startPendingJobs();
        }
    }
private:
    std::weak_ptr<Project> mProject;
};

Project::Project(const Path &path)
    : mPath(path), mState(Unloaded), mJobCounter(0)
{
    mWatcher.modified().connect(std::bind(&Project::onFileModified, this, std::placeholders::_1));
    mWatcher.removed().connect(std::bind(&Project::onFileModified, this, std::placeholders::_1));
    if (Server::instance()->options().options & Server::NoFileManagerWatch) {
        mWatcher.removed().connect(std::bind(&Project::reloadFileManager, this));
        mWatcher.added().connect(std::bind(&Project::reloadFileManager, this));
    }
    mSyncTimer.timeout().connect(std::bind(&Project::onTimerFired, this, std::placeholders::_1));
}

void Project::init()
{
    std::lock_guard<std::mutex> lock(mMutex);
    assert(mState == Unloaded);
    mState = Inited;
    fileManager.reset(new FileManager);
    fileManager->init(static_pointer_cast<Project>(shared_from_this()), FileManager::Asynchronous);
}

bool Project::restore()
{
    assert(state() == Loading);
    StopWatch timer;
    Path path = mPath;
    RTags::encodePath(path);
    const Path p = Server::instance()->options().dataDir + path;
    bool restoreError = false;
    FILE *f = fopen(p.constData(), "r");
    if (!f) {
        std::lock_guard<std::mutex> lock(mMutex);
        mState = Loaded;
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

        for (DependencyMap::const_iterator it = mDependencies.begin(); it != mDependencies.end(); ++it) {
            const Path dir = Location::path(it->first).parentDir();
            if (dir.isEmpty()) {
                error() << "File busted" << it->first << Location::path(it->first);
                continue;
            } else if ((Server::instance()->options().options & Server::WatchSystemPaths
                        || !dir.isSystem()) && mWatchedPaths.insert(dir)) {
                mWatcher.watch(dir);
            }
            for (Set<uint32_t>::const_iterator s = it->second.begin(); s != it->second.end(); ++s) {
                reversedDependencies[*s].insert(it->first);
            }
        }

        SourceInformationMap::iterator it = mSources.begin();
        while (it != mSources.end()) {
            if (!it->second.sourceFile.isFile()) {
                error() << it->second.sourceFile << "seems to have disappeared";
                mSources.erase(it++);
                dirty.insert(it->first);
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
        if (!dirty.isEmpty())
            startDirtyJobs(dirty);
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
    Map<Path, std::pair<Path, List<String> > > pendingCompiles;
    {
        std::lock_guard<std::mutex> lock(mMutex);
        mState = Loaded;
        pendingCompiles = std::move(mPendingCompiles);
    }
    for (Map<Path, std::pair<Path, List<String> > >::const_iterator it = pendingCompiles.begin(); it != pendingCompiles.end(); ++it) {
        index(it->first, it->second.first, it->second.second);
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
            fileManager->init(static_pointer_cast<Project>(shared_from_this()),
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
    for (Map<uint32_t, shared_ptr<IndexerJob> >::const_iterator it = mJobs.begin(); it != mJobs.end(); ++it) {
        it->second->abort();
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
    mPendingCompiles.clear();
    mPendingJobs.clear();

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

void Project::onJobFinished(const shared_ptr<IndexerJob> &job)
{
    PendingJob pending;
    const Path currentFile = Server::instance()->currentFile();
    bool startPending = false;
    {
        std::lock_guard<std::mutex> lock(mMutex);

        const uint32_t fileId = job->fileId();
        if (job->isAborted()) {
            mVisitedFiles -= job->visitedFiles();
            --mJobCounter;
            pending = mPendingJobs.take(fileId, &startPending);
            if (mJobs.value(fileId) == job)
                mJobs.remove(fileId);
        } else {
            assert(mJobs.value(fileId) == job);
            mJobs.remove(fileId);

            shared_ptr<IndexData> data = job->data();
            mPendingData[fileId] = data;
            if (data->type == IndexData::ClangType) {
                shared_ptr<IndexDataClang> clangData = static_pointer_cast<IndexDataClang>(data);
                if (Server::instance()->options().completionCacheSize > 0 && clangData->unit) {
                    const SourceInformation sourceInfo = job->sourceInformation();
                    if (currentFile == sourceInfo.sourceFile) {
                        shared_ptr<ReparseJob> rj(new ReparseJob(clangData->unit,
                                                                 clangData->index,
                                                                 sourceInfo.sourceFile,
                                                                 sourceInfo.args,
                                                                 static_pointer_cast<IndexerJobClang>(job)->contents(),
                                                                 static_pointer_cast<Project>(shared_from_this())));
                        clangData->index = 0;
                        clangData->unit = 0;
                        Server::instance()->startIndexerJob(rj);
                    } else {
                        addCachedUnit(sourceInfo.sourceFile, sourceInfo.args,
                                      clangData->index, clangData->unit, 1);
                        clangData->index = 0;
                        clangData->unit = 0;
                    }
                }
                clangData->clear();
            }

            const int idx = mJobCounter - mJobs.size();

            mSources[fileId].parsed = job->parseTime();
            if (testLog(RTags::CompilationErrorXml))
                log(RTags::CompilationErrorXml, "<?xml version=\"1.0\" encoding=\"utf-8\"?><progress index=\"%d\" total=\"%d\"></progress>",
                    idx, mJobCounter);

            error("[%3d%%] %d/%d %s %s.",
                  static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
                  String::formatTime(time(0), String::Time).constData(),
                  data->message.constData());

            if (mJobs.isEmpty()) {
                mSyncTimer.restart(job->type() == IndexerJob::Dirty ? 0 : SyncTimeout, Timer::SingleShot);
            }
        }
    }
    if (startPending)
        index(pending.source, pending.type);
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

void Project::index(const SourceInformation &c, IndexerJob::Type type)
{
    std::lock_guard<std::mutex> lock(mMutex);
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(c.sourceFile.constData(), fileFilter))
        return;
    const uint32_t fileId = Location::insertFile(c.sourceFile);
    shared_ptr<IndexerJob> &job = mJobs[fileId];
    if (job) {
        if (job->abortIfStarted()) {
            const PendingJob pending = { c, type };
            mPendingJobs[fileId] = pending;
        }
        return;
    }
    shared_ptr<Project> project = static_pointer_cast<Project>(shared_from_this());

    mSources[fileId] = c;
    mPendingData.remove(fileId);

    if (!mJobCounter++)
        mTimer.start();

    job = Server::instance()->factory().createJob(project, type, c);
    if (!job) {
        error() << "Failed to create job for" << c;
        mJobs.erase(fileId);
        return;
    }
    mSyncTimer.stop();

    Server::instance()->startIndexerJob(job);
}

static inline bool endsWith(const char *haystack, int haystackLen, const char *needle)
{
    const int needleLen = strlen(needle);
    return needleLen <= haystackLen && !strcmp(haystack + haystackLen - needleLen, needle);
}

static inline Path resolveCompiler(const Path &compiler)
{
    Path resolved;
    const char *linkFn;
    const char *fn;
    int fnLen;
    if (compiler.isSymLink()) {
        resolved = compiler.resolved();
        linkFn = resolved.fileName();
        fn = compiler.fileName(&fnLen);
    } else {
        linkFn = fn = compiler.fileName(&fnLen);
    }
    if (!strcmp(linkFn, "gcc-rtags-wrapper.sh") || !strcmp(linkFn, "icecc")) {
        const char *path = getenv("PATH");
        const char *last = path;
        bool done = false;
        bool found = false;
        char buf[PATH_MAX];
        while (!done) {
            switch (*path) {
            case '\0':
                done = true;
            case ':': {
                int len = (path - last);
                if (len > 0 && len + 2 + fnLen < static_cast<int>(sizeof(buf))) {
                    memcpy(buf, last, len);
                    buf[len] = '\0';
                    if (buf[len - 1] != '/')
                        buf[len++] = '/';
                    strcpy(buf + len, fn);
                    if (!access(buf, F_OK|X_OK)) {
                        if (buf == compiler) {
                            found = true;
                        } else if (found) {
                            char res[PATH_MAX];
                            buf[len + fnLen] = '\0';
                            if (realpath(buf, res)) {
                                len = strlen(res);
                                if (!endsWith(res, len, "/gcc-rtags-wrapper.sh") && !endsWith(res, len, "/icecc")) {
                                    return Path(res, len);
                                }
                                // ignore if it there's another wrapper thing in the path
                            } else {
                                return Path(buf, len + fnLen);
                            }
                        }
                    }
                }
                last = path + 1;
                break; }
            default:
                break;
            }
            ++path;
        }
    }
    if (resolved.isEmpty())
        return compiler.resolved();
    return resolved;
}

bool Project::index(const Path &sourceFile, const Path &cc, const List<String> &args)
{
    {
        std::lock_guard<std::mutex> lock(mMutex);
        switch (mState) {
        case Unloaded:
            return false;
        case Inited:
        case Loading:
            mPendingCompiles[sourceFile] = std::make_pair(cc, args);
            return true;
        case Loaded:
            break;
        }
    }

    const Path compiler = resolveCompiler(cc.canonicalized());
    SourceInformation sourceInformation = sourceInfo(Location::insertFile(sourceFile));
    const bool js = args.isEmpty() && sourceFile.endsWith(".js");
    if (sourceInformation.isNull()) {
        sourceInformation.sourceFile = sourceFile;
        sourceInformation.args = args;
        sourceInformation.compiler = compiler;
    } else if (js) {
        debug() << sourceFile << " is not dirty. ignoring";
        return false;
    } else if (sourceInformation.compiler == compiler && sourceInformation.args == args) {
        debug() << sourceFile << " is not dirty. ignoring";
        return false;
    } else {
        sourceInformation.compiler = compiler;
        sourceInformation.args = args;
    }
    index(sourceInformation, IndexerJob::Makefile);
    return true;
}

void Project::onFileModified(const Path &file)
{
    const uint32_t fileId = Location::fileId(file);
    debug() << file << "was modified" << fileId;
    if (fileId) {
        Set<uint32_t> dirty;
        dirty.insert(fileId);
        startDirtyJobs(dirty);
    }
}

SourceInformationMap Project::sourceInfos() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mSources;
}

SourceInformation Project::sourceInfo(uint32_t fileId) const
{
    if (fileId) {
        std::lock_guard<std::mutex> lock(mMutex);
        return mSources.value(fileId);
    }
    return SourceInformation();
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
    {
        std::lock_guard<std::mutex> lock(mMutex);
        SourceInformationMap::iterator it = mSources.begin();
        while (it != mSources.end()) {
            if (match.match(it->second.sourceFile)) {
                const uint32_t fileId = Location::insertFile(it->second.sourceFile);
                mSources.erase(it++);
                shared_ptr<IndexerJob> job = mJobs.value(fileId);
                if (job)
                    job->abort();
                mPendingData.remove(fileId);
                mPendingJobs.remove(fileId);
                ++count;
            } else {
                ++it;
            }
        }
    }
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
        const SourceInformationMap::const_iterator found = mSources.find(*it);
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

static inline void writeErrorSymbols(const SymbolMap &symbols, ErrorSymbolMap &errorSymbols, const Map<uint32_t, int> &errors)
{
    for (Map<uint32_t, int>::const_iterator it = errors.begin(); it != errors.end(); ++it) {
        if (it->second) {
            SymbolMap &symbolsForFile = errorSymbols[it->first];
            if (symbolsForFile.isEmpty()) {
                const Location loc(it->first, 0);
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

void Project::syncDB()
{
    if (mPendingDirtyFiles.isEmpty() && mPendingData.isEmpty())
        return;
    // for (Map<uint32_t, shared_ptr<IndexData> >::iterator it = mPendingData.begin(); it != mPendingData.end(); ++it) {
    //     writeErrorSymbols(mSymbols, mErrorSymbols, it->second->errors);
    // }

    if (!mPendingDirtyFiles.isEmpty()) {
        RTags::dirtySymbols(mSymbols, mPendingDirtyFiles);
        RTags::dirtySymbolNames(mSymbolNames, mPendingDirtyFiles);
        RTags::dirtyUsr(mUsr, mPendingDirtyFiles);
        mPendingDirtyFiles.clear();
    }

    Set<uint32_t> newFiles;
    for (Map<uint32_t, shared_ptr<IndexData> >::iterator it = mPendingData.begin(); it != mPendingData.end(); ++it) {
        const shared_ptr<IndexData> &data = it->second;
        addDependencies(data->dependencies, newFiles);
        addFixIts(data->dependencies, data->fixIts);
        writeSymbols(data->symbols, mSymbols);
        writeUsr(data->usrMap, mUsr, mSymbols);
        writeReferences(data->references, mSymbols);
        writeSymbolNames(data->symbolNames, mSymbolNames);
    }
    for (Set<uint32_t>::const_iterator it = newFiles.begin(); it != newFiles.end(); ++it) {
        const Path path = Location::path(*it);
        const Path dir = path.parentDir();
        if (dir.isEmpty()) {
            error() << "Got empty parent dir for" << path << *it;
        } else if (!(Server::instance()->options().options & Server::WatchSystemPaths) && dir.isSystem()) {
            continue;
        } else if (mWatchedPaths.insert(dir)) {
            mWatcher.watch(dir);
        }
    }
    mPendingData.clear();
    if (Server::instance()->options().options & Server::Validate) {
        shared_ptr<ValidateDBJob> validate(new ValidateDBJob(static_pointer_cast<Project>(shared_from_this()), mPreviousErrors));
        Server::instance()->startQueryJob(validate);
    }
}

bool Project::isIndexed(uint32_t fileId) const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mVisitedFiles.contains(fileId) || mSources.contains(fileId);
}

SourceInformationMap Project::sources() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mSources;
}

DependencyMap Project::dependencies() const
{
    std::lock_guard<std::mutex> lock(mMutex);
    return mDependencies;
}

void Project::addCachedUnit(const Path &path, const List<String> &args, CXIndex index,
                            CXTranslationUnit unit, int parseCount) // lock always held
{
    assert(index);
    assert(unit);
    const int maxCacheSize = Server::instance()->options().completionCacheSize;
    if (!maxCacheSize) {
        clang_disposeTranslationUnit(unit);
        clang_disposeIndex(index);
        return;
    }
    CachedUnit *cachedUnit = new CachedUnit;
    cachedUnit->path = path;
    cachedUnit->index = index;
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
                               CXIndex &index, CXTranslationUnit &unit, List<String> *argsOut,
                               int *parseCount)
{
    LinkedList<CachedUnit*>::iterator it = findCachedUnit(path, args);
    if (it != mCachedUnits.end()) {
        CachedUnit *cachedUnit = *it;
        index = cachedUnit->index;
        unit = cachedUnit->unit;
        cachedUnit->unit = 0;
        cachedUnit->index = 0;
        if (argsOut)
            *argsOut = cachedUnit->arguments;
        mCachedUnits.erase(it);
        if (parseCount)
            *parseCount = cachedUnit->parseCount;
        delete cachedUnit;
        return true;
    }
    index = 0;
    unit = 0;
    if (parseCount)
        *parseCount = -1;
    return false;
}

bool Project::fetchFromCache(const Path &path, List<String> &args, CXIndex &index, CXTranslationUnit &unit, int *parseCount)
{
    std::lock_guard<std::mutex> lock(mMutex);
    return initJobFromCache(path, List<String>(), index, unit, &args, parseCount);
}

void Project::addToCache(const Path &path, const List<String> &args, CXIndex index, CXTranslationUnit unit, int parseCount)
{
    std::lock_guard<std::mutex> lock(mMutex);
    addCachedUnit(path, args, index, unit, parseCount);
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
                out.append(String::format<32>("%d-%d %s", f->start, f->end, f->text.constData()));

            } while (f != fixIts.begin());
        }
    }
    return out;
}

void Project::onTimerFired(Timer* timer)
{
    if (timer == &mSyncTimer) {
        StopWatch sw;
        syncDB();
        const int syncTime = sw.restart();
        save();
        const int saveTime = sw.elapsed();
        error() << "Jobs took" << (static_cast<double>(mTimer.elapsed()) / 1000.0) << "secs, syncing took"
                << (static_cast<double>(syncTime) / 1000.0) << " secs, saving took"
                << (static_cast<double>(saveTime) / 1000.0) << " secs, using"
                << MemoryMonitor::usage() / (1024.0 * 1024.0) << "mb of memory";
        mJobCounter = 0;
    } else {
        assert(0 && "Unexpected timer event in Project");
        timer->stop();
    }
}
void Project::onJSFilesAdded()
{
    Set<Path> jsFiles = fileManager->jsFiles();
    for (Set<Path>::const_iterator it = jsFiles.begin(); it != jsFiles.end(); ++it) {
        index(*it);
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
