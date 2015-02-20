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

#include "IndexerJob.h"
#include "Match.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "RTagsClang.h"
#include <memory>
#include <mutex>
#include <rct/FileSystemWatcher.h>
#include <rct/EmbeddedLinkedList.h>
#include <rct/LinkedList.h>
#include <rct/Path.h>
#include <rct/RegExp.h>
#include <rct/Timer.h>

class IndexData;
class FileManager;
class IndexerJob;
class RestoreThread;
class Connection;
class Dirty;
class Project : public std::enable_shared_from_this<Project>
{
public:
    Project(const Path &path);
    ~Project();
    enum State {
        Unloaded,
        Inited,
        Loaded
    };
    State state() const { return mState; }
    void init();

    enum FileManagerMode {
        FileManager_Asynchronous,
        FileManager_Synchronous
    };
    bool load(FileManagerMode mode = FileManager_Asynchronous);
    void unload();

    std::shared_ptr<FileManager> fileManager;

    Path path() const { return mPath; }

    bool match(const Match &match, bool *indexed = 0) const;

    enum FileMapType {
        Symbols,
        SymbolNames,
        Targets,
        Usrs
    };
    static String fileMapName(FileMapType type)
    {
        switch (type) {
        case Symbols:
            return "symbols";
        case SymbolNames:
            return "symnames";
        case Targets:
            return "targets";
        case Usrs:
            return "usrs";
        }
        return String();
    }
    std::shared_ptr<FileMap<String, Set<Location> > > openSymbolNames(uint32_t fileId)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<String, Set<Location> >(SymbolNames, fileId, mFileMapScope->symbolNames);
    }
    std::shared_ptr<FileMap<Location, Symbol> > openSymbols(uint32_t fileId)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<Location, Symbol>(Symbols, fileId, mFileMapScope->symbols);
    }
    std::shared_ptr<FileMap<String, Set<Location> > > openTargets(uint32_t fileId)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<String, Set<Location> >(Targets, fileId, mFileMapScope->targets);
    }
    std::shared_ptr<FileMap<String, Set<Location> > > openUsrs(uint32_t fileId)
    {
        assert(mFileMapScope);
        return mFileMapScope->openFileMap<String, Set<Location> >(Usrs, fileId, mFileMapScope->usrs);
    }

    enum DependencyMode {
        DependsOnArg,
        ArgDependsOn // slow
    };
    Set<uint32_t> dependencies(uint32_t fileId, DependencyMode mode) const;
    const Dependencies &dependencies() const { return mDependencies; }
    const Declarations &declarations() const { return mDeclarations; }

    Set<Symbol> findSymbols(const String &symbolName, uint32_t fileId = 0);
    Symbol findSymbol(const Location &location, int *index = 0);
    Set<Symbol> findTargets(const Location &location) { return findTargets(findSymbol(location)); }
    Set<Symbol> findTargets(const Symbol &symbol);
    Symbol findTarget(const Location &location) { return RTags::bestTarget(findTargets(location)); }
    Symbol findTarget(const Symbol &symbol) { return RTags::bestTarget(findTargets(symbol)); }
    Set<Symbol> findAllReferences(const Location &location) { return findAllReferences(findSymbol(location)); }
    Set<Symbol> findAllReferences(const Symbol &symbol);
    Set<Symbol> findCallers(const Location &location) { return findCallers(findSymbol(location)); }
    Set<Symbol> findCallers(const Symbol &symbol);
    Set<Symbol> findVirtuals(const Location &location) { return findVirtuals(findSymbol(location)); }
    Set<Symbol> findVirtuals(const Symbol &symbol);
    Set<String> findTargetUsrs(const Location &loc);

    Set<Symbol> findByUsr(const String &usr, uint32_t fileId, DependencyMode mode);

    Path sourceFilePath(const String &type, uint32_t fileId) const;

    enum SortFlag {
        Sort_None = 0x0,
        Sort_DeclarationOnly = 0x1,
        Sort_Reverse = 0x2
    };
    List<RTags::SortedSymbol> sort(const Set<Symbol> &symbols, unsigned int flags = Sort_None);

    const Files &files() const { return mFiles; }
    Files &files() { return mFiles; }

    const Set<uint32_t> &suspendedFiles() const;
    bool toggleSuspendFile(uint32_t file);
    bool isSuspended(uint32_t file) const;
    void clearSuspendedFiles();

    bool isIndexed(uint32_t fileId) const;

    void index(const std::shared_ptr<IndexerJob> &job);
    List<Source> sources(uint32_t fileId) const;
    bool isActiveJob(uint64_t key) { return !key || mActiveJobs.contains(key); }
    inline bool visitFile(uint32_t fileId, const Path &path, uint64_t id);
    inline void releaseFileIds(const Set<uint32_t> &fileIds);
    String fixIts(uint32_t fileId) const;
    int reindex(const Match &match, const std::shared_ptr<QueryMessage> &query);
    int remove(const Match &match);
    void onJobFinished(const std::shared_ptr<IndexerJob> &job, const std::shared_ptr<IndexData> &indexData);
    Sources sources() const { return mSources; }
    String toCompilationDatabase() const;
    Set<Path> watchedPaths() const { return mWatchedPaths; }
    bool isIndexing() const { return !mActiveJobs.isEmpty(); }
    void onFileModifiedOrRemoved(const Path &);
    Hash<uint32_t, Path> visitedFiles() const
    {
        std::lock_guard<std::mutex> lock(mMutex);
        return mVisitedFiles;
    }
    void encodeVisitedFiles(Serializer &serializer)
    {
        std::lock_guard<std::mutex> lock(mMutex);
        serializer << mVisitedFiles;
    }

    void beginScope();
    void endScope();
private:
    void removeDependencies(uint32_t fileId);
    void watch(const Path &file);
    void reloadFileManager();
    void updateDependencies(const Set<uint32_t> &visited, Dependencies &deps);
    void updateDeclarations(const Set<uint32_t> &visited, Declarations &declarations);
    void updateFixIts(const Set<uint32_t> &visited, FixIts &fixIts);
    int startDirtyJobs(Dirty *dirty, const UnsavedFiles &unsavedFiles = UnsavedFiles());
    bool save();
    void onDirtyTimeout(Timer *);

    struct FileMapScope {
        FileMapScope(const std::shared_ptr<Project> &proj, int m)
            : project(proj), openedFiles(0), max(m)
        {}

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
        void add(FileMapType t, uint32_t f);

        template <typename Key, typename Value>
        std::shared_ptr<FileMap<Key, Value> > openFileMap(FileMapType type, uint32_t fileId,
                                                          Hash<uint32_t, std::shared_ptr<FileMap<Key, Value> > > &cache)
        {
            auto it = cache.find(fileId);
            if (it != cache.end()) {
                poke(type, fileId);
                return it->second;
            }
            const Path path = project->sourceFilePath(Project::fileMapName(type), fileId);
            std::shared_ptr<FileMap<Key, Value> > fileMap(new FileMap<Key, Value>);
            String err;
            if (fileMap->load(path, &err)) {
                cache[fileId] = fileMap;
                std::shared_ptr<LRUEntry> entry(new LRUEntry(type, fileId));
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
                    }
                    --openedFiles;
                }
                assert(openedFiles <= max);
            } else {
                error() << "Failed to open" << path << Location::path(fileId) << err;
                fileMap.reset();
            }
            return fileMap;
        }

        Hash<uint32_t, std::shared_ptr<FileMap<String, Set<Location> > > > symbolNames;
        Hash<uint32_t, std::shared_ptr<FileMap<Location, Symbol> > > symbols;
        Hash<uint32_t, std::shared_ptr<FileMap<String, Set<Location> > > > targets, usrs;
        std::shared_ptr<Project> project;
        int openedFiles;
        const int max;

        EmbeddedLinkedList<std::shared_ptr<LRUEntry> > entryList;
        Map<LRUKey, std::shared_ptr<LRUEntry> > entryMap;
    };

    std::shared_ptr<FileMapScope> mFileMapScope;

    const Path mPath, mSourceFilePathBase;
    Path mProjectFilePath;
    State mState;

    Files mFiles;

    Hash<uint32_t, Path> mVisitedFiles;
    int mJobCounter, mJobsStarted;

    // key'ed on Source::key()
    Hash<uint64_t, std::shared_ptr<IndexerJob> > mActiveJobs;

    Timer mDirtyTimer;
    Set<uint32_t> mPendingDirtyFiles;

    StopWatch mTimer;
    FileSystemWatcher mWatcher;
    Dependencies mDependencies;
    Declarations mDeclarations;
    Sources mSources;
    Set<Path> mWatchedPaths;
    FixIts mFixIts;

    Set<uint32_t> mSuspendedFiles;

    mutable std::mutex mMutex;
};

inline bool Project::visitFile(uint32_t visitFileId, const Path &path, uint64_t key)
{
    std::lock_guard<std::mutex> lock(mMutex);
    assert(visitFileId);
    Path &p = mVisitedFiles[visitFileId];
    if (p.isEmpty()) {
        p = path;
        if (key) {
            assert(mActiveJobs.contains(key));
            std::shared_ptr<IndexerJob> &job = mActiveJobs[key];
            assert(job);
            job->visited.insert(visitFileId);
        }
        return true;
    }
    return false;
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

inline void Project::removeDependencies(uint32_t fileId)
{
    mDependencies.remove(fileId);
    for (auto it = mDependencies.begin(); it != mDependencies.end(); ++it) {
        it->second.remove(fileId);
    }
}

inline Path Project::sourceFilePath(const String &type, uint32_t fileId) const
{
    Path ret = mSourceFilePathBase;
    ret << fileId << '/' << type;
    return ret;
}

#endif
