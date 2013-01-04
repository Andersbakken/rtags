#ifndef Server_h
#define Server_h

#include "FileSystemWatcher.h"
#include "ByteArray.h"
#include "List.h"
#include "Map.h"
#include "Indexer.h"
#include "QueryMessage.h"
#include "Connection.h"
#include "ThreadPool.h"
#include "RTags.h"
#include "EventReceiver.h"
#include "FileManager.h"
#include "Project.h"
#include "ScanJob.h"

class Connection;
class Indexer;
class Message;
class ErrorMessage;
class OutputMessage;
class ProjectMessage;
class LocalServer;
class GccArguments;
class Job;
class Server : public EventReceiver
{
public:
    enum { DatabaseVersion = 11 };

    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x00,
        NoClangIncludePath = 0x01,
        Validate = 0x02,
        ClearProjects = 0x04,
        NoWall = 0x08,
        IgnorePrintfFixits = 0x10,
        NoUnlimitedErrors = 0x20
    };
    ThreadPool *threadPool() const { return mIndexerThreadPool; }
    void startQueryJob(const shared_ptr<Job> &job);
    void startIndexerJob(const shared_ptr<IndexerJob> &job, int priority);
    struct Options {
        Options() : options(0), threadCount(0), completionCacheSize(0) {}
        Path projectsFile, socketFile, dataDir;
        unsigned options;
        int threadCount;
        int completionCacheSize;
        List<ByteArray> defaultArguments, excludeFilter;
    };
    bool init(const Options &options);
    const List<ByteArray> &excludeFilter() const { return mOptions.excludeFilter; }
    const Path &clangPath() const { return mClangPath; }
    const Options &options() const { return mOptions; }
    static Path findProjectRoot(const Path &path);
private:
    bool selectProject(const Match &match, Connection *conn);
    bool updateProject(const List<ByteArray> &projects);
    void onJobsComplete(shared_ptr<Indexer> indexer, int count);
    void onJobStarted(shared_ptr<Indexer> indexer, Path path);

    bool isCompletionStream(Connection* conn) const;

    static void saveTimerCallback(int id, void *userData);

    void save(const shared_ptr<Indexer> &indexer);
    void restore();
    void clear();
    void onNewConnection();
    signalslot::Signal2<int, const List<ByteArray> &> &complete() { return mComplete; }
    shared_ptr<Project> setCurrentProject(const Path &path);
    void event(const Event *event);
    void processSourceFile(GccArguments args, Path srcRoot);
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDestroyed(Connection *o);
    void clearProjects();
    void handleProjectMessage(ProjectMessage *message, Connection *conn);
    void handleCompletionMessage(CompletionMessage *message, Connection *conn);
    void handleCompletionStream(CompletionMessage *message, Connection *conn);
    void handleQueryMessage(QueryMessage *message, Connection *conn);
    void handleErrorMessage(ErrorMessage *message, Connection *conn);
    void handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn);
    void followLocation(const QueryMessage &query, Connection *conn);
    void cursorInfo(const QueryMessage &query, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void jobCount(const QueryMessage &query, Connection *conn);
    void referencesForLocation(const QueryMessage &query, Connection *conn);
    void referencesForName(const QueryMessage &query, Connection *conn);
    void findSymbols(const QueryMessage &query, Connection *conn);
    void listSymbols(const QueryMessage &query, Connection *conn);
    void status(const QueryMessage &query, Connection *conn);
    void isIndexed(const QueryMessage &query, Connection *conn);
    void hasFileManager(const QueryMessage &query, Connection *conn);
    void preprocessFile(const QueryMessage &query, Connection *conn);
    void findFile(const QueryMessage &query, Connection *conn);
    void dumpFile(const QueryMessage &query, Connection *conn);
    void removeProject(const QueryMessage &query, Connection *conn);
    void reloadProjects(const QueryMessage &query, Connection *conn);
    void project(const QueryMessage &query, Connection *conn);
    void clearProjects(const QueryMessage &query, Connection *conn);
    void shutdown(const QueryMessage &query, Connection *conn);
    int nextId();
    void reindex(const QueryMessage &query, Connection *conn);
    shared_ptr<Project> updateProjectForLocation(const Location &location);
    shared_ptr<Project> updateProjectForLocation(const Path &path);
    void writeProjects();
    shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    void removeProject(const Path &key);
    void unloadProject(const Path &key);
    void reloadProjects();
    void onCompletionStreamDisconnected(LocalClient *client);
    shared_ptr<Project> addProject(const Path &path);
    void loadProject(shared_ptr<Project> &project);
    void onCompletionJobFinished(Path path);
    void startCompletion(const Path &path, int line, int column, int pos, const ByteArray &contents, Connection *conn);

    typedef Map<Path, shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    LocalServer *mServer;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;

    ThreadPool *mIndexerThreadPool;
    ThreadPool mQueryThreadPool;
    signalslot::Signal2<int, const List<ByteArray> &> mComplete;
    Path mClangPath;

    Map<shared_ptr<Indexer>, int> mSaveTimers;

    Map<LocalClient*, Connection*> mCompletionStreams;
    struct PendingCompletion
    {
        PendingCompletion()
            : line(-1), column(-1), pos(-1), connection(0)
        {}
        int line, column, pos;
        ByteArray contents;
        Connection *connection;
    };
    Map<Path, PendingCompletion> mPendingCompletions;
    Set<Path> mActiveCompletions;

    bool mRestoreProjects;

    friend class CommandProcess;
};

#endif
