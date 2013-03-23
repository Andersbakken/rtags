#ifndef Server_h
#define Server_h

#include "QueryMessage.h"
#include "CompileMessage.h"
#include "CreateOutputMessage.h"
#include "CompletionMessage.h"
#include "FileManager.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "ScanJob.h"
#include <rct/Connection.h>
#include <rct/EventReceiver.h>
#include <rct/FileSystemWatcher.h>
#include <rct/List.h>
#include <rct/Map.h>
#include <rct/String.h>
#include <rct/ThreadPool.h>

class Connection;
class Message;
class ErrorMessage;
class OutputMessage;
class CompileMessage;
class SocketServer;
class GccArguments;
class Job;
class TimerEvent;
class Project;
class IndexerJob;
class Server : public EventReceiver
{
public:
    enum { DatabaseVersion = 18 };

    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x000,
        ClangIncludePath = 0x001,
        Validate = 0x002,
        ClearProjects = 0x004,
        Wall = 0x008,
        IgnorePrintfFixits = 0x010,
        UnlimitedErrors = 0x020,
        SpellChecking = 0x040,
        AllowMultipleBuildsForSameCompiler = 0x080,
        NoStartupCurrentProject = 0x100
    };
    ThreadPool *threadPool() const { return mIndexerThreadPool; }
    void startQueryJob(const shared_ptr<Job> &job);
    void startIndexerJob(const shared_ptr<IndexerJob> &job);
    struct Options {
        Options() : options(0), threadCount(0), completionCacheSize(0), unloadTimer(0) {}
        Path socketFile, dataDir;
        unsigned options;
        int threadCount, completionCacheSize, unloadTimer;
        List<String> defaultArguments, excludeFilters;
    };
    bool init(const Options &options);
    const List<String> &excludeFilters() const { return mOptions.excludeFilters; }
    const Path &clangPath() const { return mClangPath; }
    const Options &options() const { return mOptions; }
    bool saveFileIds() const;
private:
    bool selectProject(const Match &match, Connection *conn);
    bool updateProject(const List<String> &projects);

    bool isCompletionStream(Connection* conn) const;

    void timerEvent(TimerEvent *event);

    void restoreFileIds();
    void clear();
    void onNewConnection();
    signalslot::Signal2<int, const List<String> &> &complete() { return mComplete; }
    shared_ptr<Project> setCurrentProject(const Path &path);
    void event(const Event *event);
    void processSourceFile(GccArguments args);
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDestroyed(Connection *o);
    void clearProjects();
    void handleCompileMessage(CompileMessage *message, Connection *conn);
    void handleCompletionMessage(CompletionMessage *message, Connection *conn);
    void handleCompletionStream(CompletionMessage *message, Connection *conn);
    void handleQueryMessage(QueryMessage *message, Connection *conn);
    void handleErrorMessage(ErrorMessage *message, Connection *conn);
    void handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn);
    void isIndexing(const QueryMessage &, Connection *conn);
    void removeFile(const QueryMessage &query, Connection *conn);
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
    void shutdown(const QueryMessage &query, Connection *conn);
    void builds(const QueryMessage &query, Connection *conn);
    int nextId();
    void reindex(const QueryMessage &query, Connection *conn);
    shared_ptr<Project> updateProjectForLocation(const Match &match);
    shared_ptr<Project> updateProjectForLocation(const Location &location);
    shared_ptr<Project> updateProjectForLocation(const Path &path);
    shared_ptr<Project> currentProject() const
    {
        MutexLocker lock(&mMutex);
        return mCurrentProject.lock();
    }
    int reloadProjects();
    void onCompletionStreamDisconnected(SocketClient *client);
    shared_ptr<Project> addProject(const Path &path);
    void loadProject(shared_ptr<Project> &project);
    void onCompletionJobFinished(Path path);
    void startCompletion(const Path &path, int line, int column, int pos, const String &contents, Connection *conn);

    typedef Map<Path, shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    SocketServer *mServer;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;

    ThreadPool *mIndexerThreadPool;
    ThreadPool mQueryThreadPool;
    signalslot::Signal2<int, const List<String> &> mComplete;
    Path mClangPath;

    Map<SocketClient*, Connection*> mCompletionStreams;
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

    bool mRestoreProjects;
    Timer mUnloadTimer;

    mutable Mutex mMutex;

    friend class CommandProcess;
};

#endif
