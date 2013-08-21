#ifndef Server_h
#define Server_h

#include "QueryMessage.h"
#include "CompileMessage.h"
#include "CreateOutputMessage.h"
#include "CompletionMessage.h"
#include "FileManager.h"
#include "QueryMessage.h"
#include "RTagsClang.h"
#include "RTags.h"
#include "ScanJob.h"
#include "RTagsPluginFactory.h"
#include <rct/Connection.h>
#include <rct/FileSystemWatcher.h>
#include <rct/List.h>
#include <rct/Map.h>
#include <rct/String.h>
#include <rct/Timer.h>
#include <rct/ThreadPool.h>
#include <rct/SocketServer.h>
#include <mutex>

class Connection;
class Message;
class ErrorMessage;
class OutputMessage;
class CompileMessage;
class GccArguments;
class Job;
class JobOutput;
class Project;
class IndexerJob;
class Server
{
public:
    enum { DatabaseVersion = 26 };

    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x0000,
        NoBuiltinIncludes = 0x0001,
        Validate = 0x0002,
        ClearProjects = 0x0004,
        Wall = 0x0008,
        IgnorePrintfFixits = 0x0010,
        UnlimitedErrors = 0x0020,
        SpellChecking = 0x0040,
        AllowMultipleBuilds = 0x0080,
        NoStartupCurrentProject = 0x0100,
        WatchSystemPaths = 0x0200,
        NoFileManagerWatch = 0x0400,
        NoEsprima = 0x0800,
        UseCompilerFlags = 0x1000
    };
    ThreadPool *threadPool() const { return mIndexerThreadPool; }
    void startQueryJob(const std::shared_ptr<Job> &job);
    void startIndexerJob(const std::shared_ptr<ThreadPool::Job> &job);
    struct Options {
        Options()
            : options(0), threadCount(0), completionCacheSize(0), unloadTimer(0), clearCompletionCacheInterval(0)
        {}
        Path socketFile, dataDir;
        unsigned options;
        int threadCount, completionCacheSize, unloadTimer, clearCompletionCacheInterval;
        List<String> defaultArguments, excludeFilters;
        Set<Path> ignoredCompilers;
    };
    bool init(const Options &options);
    const Options &options() const { return mOptions; }
    uint32_t currentFileId() const { std::lock_guard<std::mutex> lock(mMutex); return mCurrentFileId; }
    bool saveFileIds() const;
    RTagsPluginFactory &factory() { return mPluginFactory; }
    void onJobOutput(JobOutput&& out);
    CXIndex clangIndex() const { return mIndex; }
private:
    bool selectProject(const Match &match, Connection *conn, unsigned int queryFlags);
    bool updateProject(const List<String> &projects, unsigned int queryFlags);

    bool isCompletionStream(Connection* conn) const;

    void clearCompletionCache();
    void restoreFileIds();
    void clear();
    void onNewConnection();
    Signal<std::function<void(int, const List<String> &)> > &complete() { return mComplete; }
    std::shared_ptr<Project> setCurrentProject(const Path &path, unsigned int queryFlags = 0);
    std::shared_ptr<Project> setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags = 0);
    void index(const GccArguments &args, const List<String> &projects);
    void onUnload();
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDisconnected(Connection *o);
    void clearProjects();
    void handleCompileMessage(const CompileMessage &message, Connection *conn);
    void handleCompletionMessage(const CompletionMessage &message, Connection *conn);
    void handleCompletionStream(const CompletionMessage &message, Connection *conn);
    void handleQueryMessage(const QueryMessage &message, Connection *conn);
    void handleErrorMessage(const ErrorMessage &message, Connection *conn);
    void handleCreateOutputMessage(const CreateOutputMessage &message, Connection *conn);
    void isIndexing(const QueryMessage &, Connection *conn);
    void removeFile(const QueryMessage &query, Connection *conn);
    void codeCompletionEnabled(const QueryMessage &query, Connection *conn);
    void followLocation(const QueryMessage &query, Connection *conn);
    void cursorInfo(const QueryMessage &query, Connection *conn);
    void dependencies(const QueryMessage &query, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void JSON(const QueryMessage &query, Connection *conn);
    void jobCount(const QueryMessage &query, Connection *conn);
    void referencesForLocation(const QueryMessage &query, Connection *conn);
    void referencesForName(const QueryMessage &query, Connection *conn);
    void findSymbols(const QueryMessage &query, Connection *conn);
    void listSymbols(const QueryMessage &query, Connection *conn);
    void status(const QueryMessage &query, Connection *conn);
    void isIndexed(const QueryMessage &query, Connection *conn);
    void hasFileManager(const QueryMessage &query, Connection *conn);
    void reloadFileManager(const QueryMessage &query, Connection *conn);
    void preprocessFile(const QueryMessage &query, Connection *conn);
    void findFile(const QueryMessage &query, Connection *conn);
    void dumpFile(const QueryMessage &query, Connection *conn);
    void removeProject(const QueryMessage &query, Connection *conn);
    void reloadProjects(const QueryMessage &query, Connection *conn);
    void project(const QueryMessage &query, Connection *conn);
    void clearProjects(const QueryMessage &query, Connection *conn);
    void loadCompilationDatabase(const QueryMessage &query, Connection *conn);
    void shutdown(const QueryMessage &query, Connection *conn);
    void builds(const QueryMessage &query, Connection *conn);
    void suspendFile(const QueryMessage &query, Connection *conn);
    int nextId();
    void reindex(const QueryMessage &query, Connection *conn);
    std::shared_ptr<Project> updateProjectForLocation(const Match &match);
    void setupCurrentProjectFile();
    std::shared_ptr<Project> currentProject() const
    {
        std::lock_guard<std::mutex> lock(mMutex);
        return mCurrentProject.lock();
    }
    int reloadProjects();
    void onCompletionStreamDisconnected(const SocketClient::SharedPtr& client);
    std::shared_ptr<Project> addProject(const Path &path);
    void onCompletionJobFinished(Path path, int id);
    void startCompletion(const Path &path, int line, int column, int pos, const String &contents, Connection *conn);

    typedef Map<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    SocketServer::SharedPtr mServer;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;

    ThreadPool *mIndexerThreadPool, *mQueryThreadPool;
    Signal<std::function<void(int, const List<String> &)> > mComplete;

    Map<SocketClient::SharedPtr, Connection*> mCompletionStreams;
    struct PendingCompletion
    {
        PendingCompletion()
            : line(-1), column(-1), pos(-1), connection(0)
        {}
        int line, column, pos;
        String contents;
        Connection *connection;
    };
    Map<Path, PendingCompletion> mPendingCompletions;
    Set<Path> mActiveCompletions;

    Timer mUnloadTimer, mClearCompletionCacheTimer;

    RTagsPluginFactory mPluginFactory;

    uint32_t mCurrentFileId;

    mutable std::mutex mMutex;

    CXIndex mIndex;
};

#endif
