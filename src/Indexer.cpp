#include "Indexer.h"

#include "ValidateDBJob.h"
#include "IndexerJob.h"
#include "Log.h"
#include "MemoryMonitor.h"
#include "Path.h"
#include "RTags.h"
#include "ReadLocker.h"
#include "RegExp.h"
#include "Server.h"
#include "WriteLocker.h"
#include <math.h>

Indexer::Indexer(const shared_ptr<Project> &proj, bool validate)
    : mJobCounter(0), mInMakefile(false), mModifiedFilesTimerId(-1), mTimerRunning(false), mProject(proj), mValidate(validate)
{
    mWatcher.modified().connect(this, &Indexer::onFileModified);
}

static inline bool isFile(uint32_t fileId)
{
    return Location::path(fileId).isFile();
}

void Indexer::onJobFinished(IndexerJob *job)
{
    MutexLocker lock(&mMutex);
    const uint32_t fileId = job->fileId();
    mVisitedFilesByJob.remove(job);
    if (mJobs.value(fileId) != job) {
        return;
    }
    mJobs.remove(fileId);
    if (job->isAborted()) {
        return;
    }
    shared_ptr<IndexData> data = job->data();
    mPendingData[fileId] = data;

    const int idx = mJobCounter - mJobs.size();

    error("[%3d%%] %d/%d %s %s. Pending jobs %d. %d mb mem.",
          static_cast<int>(round((double(idx) / double(mJobCounter)) * 100.0)), idx, mJobCounter,
          RTags::timeToString(time(0), RTags::Time).constData(),
          data->message.constData(), mJobs.size(), int((MemoryMonitor::usage() / (1024 * 1024))));

    checkFinished();
}

void Indexer::index(const Path &input, const List<ByteArray> &arguments, unsigned indexerJobFlags)
{
    MutexLocker locker(&mMutex);
    static const char *fileFilter = getenv("RTAGS_FILE_FILTER");
    if (fileFilter && !strstr(input.constData(), fileFilter))
        return;

    const uint32_t fileId = Location::insertFile(input);
    mCompileArguments[fileId] = arguments;
    IndexerJob *&job = mJobs[fileId];
    if (job) {
        job->abort();
        mVisitedFiles -= mVisitedFilesByJob.take(job);
    }
    mPendingData.remove(fileId);

    job = new IndexerJob(shared_from_this(), indexerJobFlags, input, arguments);

    ++mJobCounter;
    if (!mTimerRunning) {
        mTimerRunning = true;
        mTimer.start();
    }

    Server::instance()->threadPool()->start(job, job->priority());
}

void Indexer::onFileModified(const Path &file)
{
    // error() << file << "was modified";
    const uint32_t fileId = Location::fileId(file);
    if (!fileId)
        return;
    mModifiedFiles.insert(fileId);
    if (mModifiedFilesTimerId != -1) {
        EventLoop::instance()->removeTimer(mModifiedFilesTimerId);
        mModifiedFilesTimerId = -1;
    }
    enum { Timeout = 100 };
    mModifiedFilesTimerId = EventLoop::instance()->addTimer(Timeout, &Indexer::onFilesModifiedTimeout, this);
}

List<ByteArray> Indexer::compileArguments(uint32_t fileId) const
{
    if (fileId) {
        MutexLocker lock(&mMutex);
        return mCompileArguments.value(fileId);
    }
    return List<ByteArray>();
}

void Indexer::addDependencies(const DependencyMap &deps, Set<uint32_t> &newFiles)
{
    Timer timer;

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

Set<uint32_t> Indexer::dependencies(uint32_t fileId) const
{
    MutexLocker lock(&mMutex);
    return mDependencies.value(fileId);
}

ByteArray Indexer::fixIts(const Path &path) const
{
    uint32_t fileId = Location::fileId(path);
    if (!fileId)
        return ByteArray();
    MutexLocker lock(&mMutex);
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
    MutexLocker lock(&mMutex);
    return mErrors.value(fileId);
}

void Indexer::addDiagnostics(const DiagnosticsMap &diagnostics, const FixitMap &fixIts)
{
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

int Indexer::reindex(const ByteArray &pattern, bool regexp)
{
    Set<uint32_t> dirty;
    {
        MutexLocker lock(&mMutex);
        RegExp rx;
        if (regexp)
            rx = RegExp(pattern);

        const DependencyMap::const_iterator end = mDependencies.end();
        for (DependencyMap::const_iterator it = mDependencies.begin(); it != end; ++it) {
            if (!mPendingDirtyFiles.contains(it->first)) {
                if (pattern.isEmpty()) {
                    dirty.insert(it->first);
                } else {
                    const Path path = Location::path(it->first);
                    if (regexp) {
                        if (rx.indexIn(path) != -1)
                            dirty.insert(it->first);
                    } else if (path.contains(pattern)) {
                        dirty.insert(it->first);
                    }
                }
            }
        }
        if (dirty.isEmpty())
            return 0;
        mModifiedFiles += dirty;
    }
    onFilesModifiedTimeout();
    return dirty.size();
}

void Indexer::onValidateDBJobErrors(const Set<Location> &errors)
{
    MutexLocker lock(&mMutex);
    mPreviousErrors = errors;
}

void Indexer::onFilesModifiedTimeout()
{
    Set<uint32_t> dirtyFiles;
    Map<Path, List<ByteArray> > toIndex;
    {
        MutexLocker lock(&mMutex);
        for (Set<uint32_t>::const_iterator it = mModifiedFiles.begin(); it != mModifiedFiles.end(); ++it) {
            dirtyFiles.insert(*it);
            dirtyFiles.unite(mDependencies.at(*it));
        }
        mVisitedFiles -= dirtyFiles;
        mPendingDirtyFiles.unite(dirtyFiles);
        mModifiedFiles.clear();
    }
    for (Set<uint32_t>::const_iterator it = dirtyFiles.begin(); it != dirtyFiles.end(); ++it) {
        const CompileArgumentsMap::const_iterator found = mCompileArguments.find(*it);
        if (found != mCompileArguments.end()) {
            const Path path = Location::path(*it);
            index(path, found->second, IndexerJob::Dirty);
        }
    }
}

static inline void writeSymbolNames(const SymbolNameMap &symbolNames, Scope<SymbolNameMap&> &cur)
{
    SymbolNameMap &current = cur.data();
    SymbolNameMap::const_iterator it = symbolNames.begin();
    const SymbolNameMap::const_iterator end = symbolNames.end();
    while (it != end) {
        Set<Location> &value = current[it->first];
        value.unite(it->second);
        ++it;
    }
}

static inline void writeCursors(const SymbolMap &symbols, Scope<SymbolMap&> &cur)
{
    if (!symbols.isEmpty()) {
        SymbolMap &current = cur.data();
        if (current.isEmpty()) {
            current = symbols;
        } else {
            SymbolMap::const_iterator it = symbols.begin();
            const SymbolMap::const_iterator end = symbols.end();
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

static inline void writeReferences(const ReferenceMap &references, Scope<SymbolMap&> &cur)
{
    SymbolMap &symbols = cur.data();
    if (!references.isEmpty()) {
        const ReferenceMap::const_iterator end = references.end();
        for (ReferenceMap::const_iterator it = references.begin(); it != end; ++it) {
            const Map<Location, RTags::ReferenceType> &refs = it->second;
            for (Map<Location, RTags::ReferenceType>::const_iterator rit = refs.begin(); rit != refs.end(); ++rit) {
                CursorInfo &ci = symbols[rit->first];
                if (rit->second != RTags::NormalReference) {
                    CursorInfo &other = symbols[it->first];
                    // error() << "trying to join" << it->first << "and" << it->second.front();
                    if (other.target.isNull())
                        other.target = rit->first;
                    if (ci.target.isNull())
                        ci.target = it->first;
                } else {
                    ci.references.insert(it->first);
                }
            }
        }
    }
}


void Indexer::write()
{
    shared_ptr<Project> proj = project();
    Scope<SymbolMap&> symbols = proj->lockSymbolsForWrite();
    Scope<SymbolNameMap&> symbolNames = proj->lockSymbolNamesForWrite();
    if (!mPendingDirtyFiles.isEmpty()) {
        RTags::dirtySymbols(symbols.data(), mPendingDirtyFiles);
        RTags::dirtySymbolNames(symbolNames.data(), mPendingDirtyFiles);
        mPendingDirtyFiles.clear();
    }

    Set<uint32_t> newFiles;
    for (Map<uint32_t, shared_ptr<IndexData> >::iterator it = mPendingData.begin(); it != mPendingData.end(); ++it) {
        const shared_ptr<IndexData> &data = it->second;
        addDependencies(data->dependencies, newFiles);
        addDiagnostics(data->diagnostics, data->fixIts);
        writeCursors(data->symbols, symbols);
        writeReferences(data->references, symbols);
        writeSymbolNames(data->symbolNames, symbolNames);
    }
    Timer timer;
    for (Set<uint32_t>::const_iterator it = newFiles.begin(); it != newFiles.end(); ++it) {
        const Path dir = Location::path(*it).parentDir();
        if (mWatchedPaths.insert(dir)) {
            mWatcher.watch(dir);
        }
    }
    mPendingData.clear();
}

void Indexer::beginMakefile()
{
    MutexLocker lock(&mMutex);
    mInMakefile = true;
}

void Indexer::endMakefile()
{
    MutexLocker lock(&mMutex);
    mInMakefile = false;
    checkFinished();
}

void Indexer::checkFinished() // lock always held
{
    if (mJobs.isEmpty() && !mInMakefile) {
        mTimerRunning = false;
        const int elapsed = mTimer.restart();
        write();
        mJobCounter = 0;
        error() << "Jobs took" << ((double)(elapsed) / 1000.0) << "secs, writing took"
                << ((double)(mTimer.elapsed()) / 1000.0) << " secs, using"
                << MemoryMonitor::usage() / (1024.0 * 1024.0) << "mb of memory";
        jobsComplete()(this);
        if (mValidate) {
            ValidateDBJob *validateJob = new ValidateDBJob(project(), mPreviousErrors);
            validateJob->errors().connect(this, &Indexer::onValidateDBJobErrors);
            Server::instance()->startJob(validateJob);
        }
    }
}
