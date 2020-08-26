/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef Project_h
#define Project_h

#include <assert.h>
#include <cstdint>
#include <mutex>
#include <ctime>
#include <functional>
#include <memory>
#include <unordered_map>

#include "Diagnostic.h"
#include "FileMap.h"
#include "IndexerJob.h"
#include "IndexMessage.h"
#include "QueryMessage.h"
#include "IndexParseData.h"
#include "rct/EmbeddedLinkedList.h"
#include "rct/FileSystemWatcher.h"
#include "rct/Flags.h"
#include "rct/Path.h"
#include "rct/StopWatch.h"
#include "rct/Timer.h"
#include "rct/Serializer.h"
#include "RTags.h"
#include "Token.h"
#include "Location.h"
#include "Source.h"
#include "Symbol.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "rct/Map.h"
#include "rct/Rct.h"
#include "rct/Set.h"
#include "rct/String.h"

class Connection;
class Dirty;
class FileManager;
class IndexDataMessage;
class Match;
class RestoreThread;
struct Token;
template <typename Key, typename Value> class FileMap;

struct DependencyNode
{
    DependencyNode(uint32_t f)
        : fileId(f)
    {}
    void include(DependencyNode *dependee)
    {
        assert(!includes.contains(dependee->fileId) || includes.value(dependee->fileId) == dependee);
        includes[dependee->fileId] = dependee;
        assert(!dependee->dependents.contains(fileId) || dependee->dependents.value(fileId) == this);
        dependee->dependents[fileId] = this;
    }

    Dependencies dependents, includes;
    uint32_t fileId;
};

class Project : public std::enable_shared_from_this<Project>
{
public:
    Project(const Path &path);
    ~Project();
    bool init();

    std::shared_ptr<FileManager> fileManager() const { return mFileManager; }

    Path path() const { return mPath; }
    Path projectDataDir() const { return mProjectDataDir; }
    bool match(const Match &match, bool *indexed = nullptr) const;

    enum FileMapType {
        Symbols,
        SymbolNames,
        Targets,
        Usrs,
        Tokens
    };
    static const char *fileMapName(FileMapType type)
    {
        switch (type) {
        case Symbols: return "symbols";
        case SymbolNames: return "symnames";
        case Targets: return "targets";
        case Usrs: return "usrs";
        case Tokens: return "tokens";
        }
        return nullptr;
    }
    std::shared_ptr<FileMap<String, Set<Location> > > openSymbolNames(uint32_t fileId, String *err = nullptr)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<String, Set<Location> >(SymbolNames, fileId, mFileMapScope->symbolNames, err);
    }
    std::shared_ptr<FileMap<Location, Symbol> > openSymbols(uint32_t fileId, String *err = nullptr)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<Location, Symbol>(Symbols, fileId, mFileMapScope->symbols, err);
    }
    std::shared_ptr<FileMap<String, Set<Location> > > openTargets(uint32_t fileId, String *err = nullptr)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<String, Set<Location> >(Targets, fileId, mFileMapScope->targets, err);
    }
    std::shared_ptr<FileMap<String, Set<Location> > > openUsrs(uint32_t fileId, String *err = nullptr)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<String, Set<Location> >(Usrs, fileId, mFileMapScope->usrs, err);
    }

    std::shared_ptr<FileMap<uint32_t, Token> > openTokens(uint32_t fileId, String *err = nullptr)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<uint32_t, Token>(Tokens, fileId, mFileMapScope->tokens, err);
    }


    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn,
        All
    };

    Map<Symbol, size_t> findDeadFunctions(uint32_t fileId);
    Set<uint32_t> dependencies(uint32_t fileId, DependencyMode mode) const;
    bool dependsOn(uint32_t source, uint32_t header) const;
    String dumpDependencies(uint32_t fileId,
                            const List<String> &args = List<String>(),
                            Flags<QueryMessage::Flag> flags = Flags<QueryMessage::Flag>()) const;
    const Hash<uint32_t, DependencyNode*> &dependencies() const { return mDependencies; }
    DependencyNode *dependencyNode(uint32_t fileId) const { return mDependencies.value(fileId); }

    static bool readSources(const Path &path, IndexParseData &data, String *error);
    enum SymbolMatchType {
        Exact,
        Wildcard,
        Regexp,
        StartsWith
    };
    void findSymbols(const String &symbolName,
                     const std::function<void(SymbolMatchType, const String &, const Set<Location> &)> &func,
                     Flags<QueryMessage::Flag> queryFlags,
                     uint32_t fileFilter = 0);

    static bool matchSymbolName(const String &pattern, const String &symbolName, String::CaseSensitivity cs)
    {
        return Rct::wildCmp(pattern.constData(), symbolName.constData(), cs);
    }

    Symbol findSymbol(Location location, int *index = nullptr);
    Set<Symbol> findTargets(Location location) { return findTargets(findSymbol(location)); }
    Set<Symbol> findTargets(const Symbol &symbol);
    Symbol findTarget(Location location) { return RTags::bestTarget(findTargets(location)); }
    Symbol findTarget(const Symbol &symbol) { return RTags::bestTarget(findTargets(symbol)); }
    Set<Symbol> findAllReferences(Location location) { return findAllReferences(findSymbol(location)); }
    Set<Symbol> findAllReferences(const Symbol &symbol);
    Set<Symbol> findCallers(Location location, int max = -1) { return findCallers(findSymbol(location), max); }
    Set<Symbol> findCallers(const Symbol &symbol, int max = -1);
    Set<Symbol> findVirtuals(Location location) { return findVirtuals(findSymbol(location)); }
    Set<Symbol> findVirtuals(const Symbol &symbol);
    Set<String> findTargetUsrs(const Symbol &symbol);
    Set<String> findTargetUsrs(Location loc);
    Set<Symbol> findSubclasses(const Symbol &symbol);

    Set<Symbol> findByUsr(const String &usr, uint32_t fileId, DependencyMode mode);

    Path sourceFilePath(uint32_t fileId, const char *path = "") const;

    List<RTags::SortedSymbol> sort(const Set<Symbol> &symbols,
                                   Flags<QueryMessage::Flag> flags = Flags<QueryMessage::Flag>());

    const Files &files() const { return mFiles; }
    Files &files() { return mFiles; }

    const Set<uint32_t> &suspendedFiles() const;
    bool toggleSuspendFile(uint32_t file);
    void setSuspended(uint32_t file, bool suspended);
    bool isSuspended(uint32_t file) const;
    void clearSuspendedFiles();

    bool isIndexed(uint32_t fileId) const;

    void processParseData(IndexParseData &&data);
    const IndexParseData &indexParseData() const { return mIndexParseData; }
    void index(const std::shared_ptr<IndexerJob> &job);
    void reindex(uint32_t fileId, Flags<IndexerJob::Flag> flags);
    SourceList sources(uint32_t fileId) const;
    Source source(uint32_t fileId, int buildIndex) const;
    bool hasSource(uint32_t fileId) const;
    bool isActiveJob(uint32_t sourceFileId) { return !sourceFileId || mActiveJobs.contains(sourceFileId); }
    inline bool visitFile(uint32_t fileId, uint32_t sourceFileId);
    inline void releaseFileIds(const Set<uint32_t> &fileIds);
    String fixIts(uint32_t fileId) const;
    int reindex(const Match &match,
                const std::shared_ptr<QueryMessage> &query,
                const std::shared_ptr<Connection> &wait);
    int remove(const Match &match);
    void onJobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexDataMessage> &msg);
    String toCompileCommands() const;
    enum WatchMode {
        Watch_FileManager = 0x1,
        Watch_SourceFile = 0x2,
        Watch_Dependency = 0x4,
        Watch_CompileCommands = 0x8
    };

    void watch(const Path &dir, WatchMode mode);
    void unwatch(const Path &dir, WatchMode mode);
    void clearWatch(Flags<WatchMode> mode);
    Hash<Path, Flags<WatchMode> > watchedPaths() const { return mWatchedPaths; }

    time_t lastIdleTime() const { return mLastIdleTime; }
    bool isIndexing() const { return !mActiveJobs.isEmpty(); }
    void onFileAdded(const Path &path);
    void onFileModified(const Path &path);
    void onFileRemoved(const Path &path);
    void dumpFileMaps(const std::shared_ptr<QueryMessage> &msg, const std::shared_ptr<Connection> &conn);
    void removeSources(const Hash<uint32_t, uint32_t> &sources); // key fileid, value fileid for compile_commands.json
    void removeSource(uint32_t fileId);
    Set<uint32_t> visitedFiles() const
    {
        std::lock_guard<std::mutex> lock(mMutex);
        return mVisitedFiles;
    }
    void encodeVisitedFiles(Serializer &serializer)
    {
        std::lock_guard<std::mutex> lock(mMutex);
        serializer << static_cast<uint32_t>(mVisitedFiles.size());
        for (uint32_t fileId : mVisitedFiles) {
            serializer << fileId << Location::path(fileId);
        }
    }

    enum ScopeFlag { None = 0x0, NoValidate = 0x1 };

    class FileMapScopeScope
    {
    public:
        FileMapScopeScope(Project *p, Flags<ScopeFlag> flags = NullFlags)
            : mProject(p)
        {
            if (mProject)
                mProject->beginScope(flags);
        }
        FileMapScopeScope(const std::shared_ptr<Project> &p)
            : FileMapScopeScope(p.get())
        {}

        ~FileMapScopeScope()
        {
            if (mProject)
                mProject->endScope();
        }
    private:
        Project *mProject;
    };

    void beginScope(Flags<ScopeFlag> flags = NullFlags);
    void endScope();
    void dirty(uint32_t fileId);
    bool save();
    void prepare(uint32_t fileId);
    String estimateMemory() const;
    String diagnosticsToString(Flags<QueryMessage::Flag> flags, uint32_t fileId);
    void diagnose(uint32_t fileId);
    void diagnoseAll();
    uint32_t fileMapOptions() const;
    void fixPCH(Source &source);
    void includeCompletions(Flags<QueryMessage::Flag> flags, const std::shared_ptr<Connection> &conn, Source &&source) const;
    size_t bytesWritten() const { return mBytesWritten; }
    void destroy() { mSaveDirty = false; }
    enum VisitResult {
        Stop,
        Continue,
        Remove // not allowed for const calls
    };
    static void forEachSources(const IndexParseData &data, const std::function<VisitResult(const Sources &sources)>& cb);
    static void forEachSources(IndexParseData &data, const std::function<VisitResult(Sources &sources)>& cb);
    void forEachSources(std::function<VisitResult(const Sources &sources)> cb) const { forEachSources(mIndexParseData, cb); }
    void forEachSources(std::function<VisitResult(Sources &sources)> cb) { forEachSources(mIndexParseData, cb); }

    static void forEachSourceList(const IndexParseData &data, std::function<VisitResult(const SourceList &sources)> cb);
    static void forEachSourceList(IndexParseData &data, std::function<VisitResult(SourceList &sources)> cb);
    static void forEachSourceList(Sources &sources, const std::function<VisitResult(SourceList &source)>& cb);
    static void forEachSourceList(const Sources &sources, const std::function<VisitResult(const SourceList &source)>& cb);
    void forEachSourceList(std::function<VisitResult(const SourceList &sources)> cb) const { forEachSourceList(mIndexParseData, cb); }
    void forEachSourceList(std::function<VisitResult(SourceList &sources)> cb) { forEachSourceList(mIndexParseData, cb); }

    static void forEachSource(Sources &sources, const std::function<VisitResult(Source &source)>& cb);
    static void forEachSource(const Sources &sources, const std::function<VisitResult(const Source &source)>& cb);
    static void forEachSource(IndexParseData &data, std::function<VisitResult(Source &source)> cb);
    static void forEachSource(const IndexParseData &data, std::function<VisitResult(const Source &source)> cb);
    void forEachSource(std::function<VisitResult(const Source &source)> cb) const { forEachSource(mIndexParseData, cb); }
    void forEachSource(std::function<VisitResult(Source &source)> cb) { forEachSource(mIndexParseData, cb); }
    void validateAll();
    void updateDiagnostics(uint32_t fileId, const Diagnostics &diagnostics);
    enum CheckMode {
        Check_Init,
        Check_Recurring,
        Check_Explicit
    };
    void check(CheckMode mode);
private:
    void reloadCompileCommands();
    void onFileAddedOrModified(const Path &path, uint32_t fileId);
    void watchFile(uint32_t fileId);
    enum ValidateMode {
        StatOnly,
        Validate,
        ValidateSilent
    };
    bool validate(uint32_t fileId, ValidateMode mode, String *error = nullptr) const;
    void removeDependencies(uint32_t fileId);
    void updateDependencies(uint32_t fileId, const std::shared_ptr<IndexDataMessage> &msg);
    void loadFailed(uint32_t fileId);
    void updateFixIts(const Set<uint32_t> &visited, FixIts &fixIts);
    int startDirtyJobs(Dirty *dirty,
                       Flags<IndexerJob::Flag> type,
                       const UnsavedFiles &unsavedFiles = UnsavedFiles(),
                       const std::shared_ptr<Connection> &wait = std::shared_ptr<Connection>());
    void onDirtyTimeout(Timer *);

    struct FileMapScope {
        FileMapScope(const std::shared_ptr<Project> &proj, int m, Flags<ScopeFlag> f)
            : project(proj), openedFiles(0), totalOpened(0), max(m), loadFailed(false), flags(f)
        {}
        ~FileMapScope()
        {
            warning() << "Query opened" << totalOpened << "files for project" << project->path();
            if (loadFailed && !(flags & NoValidate))
                project->validateAll();
        }

        struct LRUKey {
            FileMapType type;
            uint32_t fileId;
            bool operator<(const LRUKey &other) const
            {
                return fileId < other.fileId || (fileId == other.fileId && type < other.type);
            }
        };
        struct LRUEntry {
            LRUEntry(FileMapType t, uint32_t f)
                : key({ t, f })
            {}
            const LRUKey key;

            std::shared_ptr<LRUEntry> next, prev;
        };

        void poke(FileMapType t, uint32_t f)
        {
            const LRUKey key = { t, f };
            auto ptr = entryMap.value(key);
            assert(ptr);
            entryList.remove(ptr);
            entryList.append(ptr);
        }

        template <typename Key, typename Value>
        std::shared_ptr<FileMap<Key, Value> > openFileMap(FileMapType type, uint32_t fileId,
                                                          Hash<uint32_t, std::shared_ptr<FileMap<Key, Value> > > &cache,
                                                          String *errPtr)
        {
            auto it = cache.find(fileId);
            if (it != cache.end()) {
                poke(type, fileId);
                return it->second;
            }
            const Path path = project->sourceFilePath(fileId, Project::fileMapName(type));
            auto fileMap = std::make_shared<FileMap<Key, Value>>();
            String err;
            if (fileMap->load(path, project->fileMapOptions(), &err)) {
                ++totalOpened;
                cache[fileId] = fileMap;
                auto entry = std::make_shared<LRUEntry>(type, fileId);
                entryList.append(entry);
                entryMap[entry->key] = entry;
                if (++openedFiles > max) {
                    const std::shared_ptr<LRUEntry> e = entryList.takeFirst();
                    assert(e);
                    entryMap.remove(e->key);
                    switch (e->key.type) {
                    case SymbolNames:
                        assert(symbolNames.contains(e->key.fileId));
                        symbolNames.remove(e->key.fileId);
                        break;
                    case Symbols:
                        assert(symbols.contains(e->key.fileId));
                        symbols.remove(e->key.fileId);
                        break;
                    case Targets:
                        assert(targets.contains(e->key.fileId));
                        targets.remove(e->key.fileId);
                        break;
                    case Usrs:
                        assert(usrs.contains(e->key.fileId));
                        usrs.remove(e->key.fileId);
                        break;
                    case Tokens:
                        assert(tokens.contains(e->key.fileId));
                        tokens.remove(e->key.fileId);
                        break;
                    }
                    --openedFiles;
                }
                assert(openedFiles <= max);
            } else {
                if (!(flags & NoValidate)) {
                    if (errPtr) {
                        *errPtr = "Failed to open: " + path + " " + Location::path(fileId) + ": " + err;
                    } else {
                        error() << "Failed to open" << path << Location::path(fileId) << err;
                    }
                }
                loadFailed = true;
                fileMap.reset();
            }
            return fileMap;
        }

        Hash<uint32_t, std::shared_ptr<FileMap<String, Set<Location> > > > symbolNames;
        Hash<uint32_t, std::shared_ptr<FileMap<Location, Symbol> > > symbols;
        Hash<uint32_t, std::shared_ptr<FileMap<String, Set<Location> > > > targets, usrs;
        Hash<uint32_t, std::shared_ptr<FileMap<uint32_t, Token> > > tokens;
        std::shared_ptr<Project> project;
        int openedFiles, totalOpened;
        const int max;
        bool loadFailed;
        Flags<ScopeFlag> flags;

        EmbeddedLinkedList<std::shared_ptr<LRUEntry> > entryList;
        Map<LRUKey, std::shared_ptr<LRUEntry> > entryMap;
    };

    std::shared_ptr<FileMapScope> mFileMapScope;

    const Path mPath, mProjectDataDir;
    Path mProjectFilePath, mSourcesFilePath;

    Files mFiles;

    Set<uint32_t> mVisitedFiles;
    int mJobCounter, mJobsStarted;

    time_t mLastIdleTime;

    Diagnostics mDiagnostics;

    Hash<uint32_t, std::shared_ptr<IndexerJob> > mActiveJobs;

    Timer mDirtyTimer, mCheckTimer;
    Set<uint32_t> mPendingDirtyFiles;

    StopWatch mTimer;
    FileSystemWatcher mWatcher;
    IndexParseData mIndexParseData;
    Hash<Path, Flags<WatchMode> > mWatchedPaths;
    std::shared_ptr<FileManager> mFileManager;
    FixIts mFixIts;

    Hash<uint32_t, DependencyNode*> mDependencies;
    Set<uint32_t> mSuspendedFiles;

    size_t mBytesWritten;
    bool mSaveDirty;

    mutable std::mutex mMutex;
};

RCT_FLAGS(Project::WatchMode);
RCT_FLAGS(Project::ScopeFlag);

inline bool Project::visitFile(uint32_t visitFileId, uint32_t id)
{
    assert(id);
    std::lock_guard<std::mutex> lock(mMutex);
    assert(visitFileId);
    assert(id);
    assert(mActiveJobs.contains(id));
    std::shared_ptr<IndexerJob> &job = mActiveJobs[id];
    assert(job);
    if (mVisitedFiles.insert(visitFileId)) {
        job->visited.insert(visitFileId);
        return true;
    }
    return job->visited.contains(visitFileId);
}

inline void Project::releaseFileIds(const Set<uint32_t> &fileIds)
{
    if (!fileIds.isEmpty()) {
        std::lock_guard<std::mutex> lock(mMutex);
        for (const auto &f : fileIds) {
            // error() << "Returning files" << Location::path(f);
            mVisitedFiles.remove(f);
        }
    }
}

inline Path Project::sourceFilePath(uint32_t fileId, const char *type) const
{
    return String::format<1024>("%s%d/%s", mProjectDataDir.constData(), fileId, type);
}

#endif
