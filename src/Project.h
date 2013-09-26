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

#ifndef Project_h
#define Project_h

#include "CursorInfo.h"
#include <rct/Path.h>
#include <rct/LinkedList.h>
#include "RTags.h"
#include "Match.h"
#include <rct/Timer.h>
#include <rct/RegExp.h>
#include <rct/FileSystemWatcher.h>
#include "IndexerJob.h"
#include <mutex>
#include <memory>

struct CachedUnit
{
    CachedUnit()
        : unit(0), parseCount(0)
    {}
    ~CachedUnit()
    {
        clear();
    }
    void clear()
    {
        if (unit) {
            clang_disposeTranslationUnit(unit);
            unit = 0;
        }

    }
    CXTranslationUnit unit;
    Path path;
    List<String> arguments;
    int parseCount;
};

class FileManager;
class IndexerJob;
class IndexData;
class Project : public std::enable_shared_from_this<Project>
{
public:
    Project(const Path &path);
    ~Project();
    enum State {
        Unloaded,
        Inited,
        Loading,
        Loaded
    };
    State state() const;
    void init();
    bool restore();
    void startPendingJobs();

    enum FileManagerMode {
        FileManager_Asynchronous,
        FileManager_Synchronous
    };
    void load(FileManagerMode mode = FileManager_Asynchronous);
    void unload();

    std::shared_ptr<FileManager> fileManager;

    Path path() const { return mPath; }

    bool match(const Match &match, bool *indexed = 0) const;

    const SymbolMap &symbols() const { return mSymbols; }
    SymbolMap &symbols() { return mSymbols; }

    const ErrorSymbolMap &errorSymbols() const { return mErrorSymbols; }
    ErrorSymbolMap &errorSymbols() { return mErrorSymbols; }

    const SymbolNameMap &symbolNames() const { return mSymbolNames; }
    SymbolNameMap &symbolNames() { return mSymbolNames; }

    Set<Location> locations(const String &symbolName, uint32_t fileId = 0) const;
    SymbolMap symbols(uint32_t fileId) const;
    enum SortFlag {
        Sort_None = 0x0,
        Sort_DeclarationOnly = 0x1,
        Sort_Reverse = 0x2
    };
    List<RTags::SortedCursor> sort(const Set<Location> &locations, unsigned int flags = Sort_None) const;

    const FilesMap &files() const { return mFiles; }
    FilesMap &files() { return mFiles; }

    const UsrMap &usrs() const { return mUsr; }
    UsrMap &usrs() { return mUsr; }

    const Set<uint32_t> &suspendedFiles() const;
    bool toggleSuspendFile(uint32_t file);
    bool isSuspended(uint32_t file) const;
    void clearSuspendedFiles();

    bool isIndexed(uint32_t fileId) const;

    // void dump(const QueryMessage &query, Connection *conn);
    bool index(const Path &sourceFile, const Path &compiler = Path(), const List<String> &args = List<String>());
    SourceInformationMap sourceInfos() const;
    SourceInformation sourceInfo(uint32_t fileId) const;
    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn // slow
    };
    Set<uint32_t> dependencies(uint32_t fileId, DependencyMode mode) const;
    bool visitFile(uint32_t fileId, uint64_t id);
    String fixIts(uint32_t fileId) const;
    int reindex(const Match &match);
    int remove(const Match &match);
    void onJobFinished(const std::shared_ptr<IndexData> &job);
    SourceInformationMap sources() const;
    DependencyMap dependencies() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool fetchFromCache(const Path &path, List<String> &args, CXTranslationUnit &unit, int *parseCount);
    void addToCache(const Path &path, const List<String> &args, CXTranslationUnit unit, int parseCount);
    void onTimerFired(Timer* event);
    bool isIndexing() const { std::lock_guard<std::mutex> lock(mMutex); return !mJobs.isEmpty(); }
    void onJSFilesAdded();
    List<std::pair<Path, List<String> > > cachedUnits() const;
private:
    void index(const SourceInformation &args, IndexType type);
    void reloadFileManager();
    bool initJobFromCache(const Path &path, const List<String> &args,
                          CXTranslationUnit &unit, List<String> *argsOut, int *parseCount);
    LinkedList<CachedUnit*>::iterator findCachedUnit(const Path &path, const List<String> &args);
    void onFileModified(const Path &);
    void addDependencies(const DependencyMap &hash, Set<uint32_t> &newFiles);
    void addFixIts(const DependencyMap &dependencies, const FixItMap &fixIts);
    void syncDB(int *dirtyTime, int *syncTime);
    void startDirtyJobs(const Set<uint32_t> &files);
    void addCachedUnit(const Path &path, const List<String> &args, CXTranslationUnit unit, int parseCount);
    bool save();
    void onValidateDBJobErrors(const Set<Location> &errors);

    const Path mPath;
    State mState;

    Hash<Path, std::pair<Path, List<String> > > mPendingCompiles;

    SymbolMap mSymbols;
    ErrorSymbolMap mErrorSymbols;
    SymbolNameMap mSymbolNames;
    UsrMap mUsr;
    FilesMap mFiles;

    enum InitMode {
        Normal,
        NoValidate,
        ForceDirty
    };

    Set<uint32_t> mVisitedFiles;

    int mJobCounter;

    mutable std::mutex mMutex;

    Hash<uint32_t, std::shared_ptr<IndexerJob> > mJobs, mDumps;
    struct PendingJob
    {
        SourceInformation source;
        IndexType type;
    };
    Hash<uint32_t, PendingJob> mPendingJobs;

    Timer mSyncTimer;

    StopWatch mTimer;

    FileSystemWatcher mWatcher;
    DependencyMap mDependencies;
    SourceInformationMap mSources;

    Set<Path> mWatchedPaths;

    FixItMap mFixIts;

    Set<Location> mPreviousErrors;

    Hash<uint32_t, std::shared_ptr<IndexData> > mPendingData;
    Set<uint32_t> mPendingDirtyFiles;

    LinkedList<CachedUnit*> mCachedUnits;
    Set<uint32_t> mSuspendedFiles;
    uint64_t mNextId;
};

inline bool Project::visitFile(uint32_t fileId, uint64_t id)
{
    std::lock_guard<std::mutex> lock(mMutex);
    if (mVisitedFiles.insert(fileId)) {
        std::shared_ptr<IndexerJob> job = mJobs.value(id);
        assert(job);
        job->visited.insert(id);
        return true;
    }
    return false;
}

#endif
