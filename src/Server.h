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
#include "GRScanJob.h"

class GRTagsMessage;
class Connection;
class Indexer;
class Message;
class ErrorMessage;
class OutputMessage;
class ProjectMessage;
class LocalServer;
class GccArguments;
class MakefileParser;
class Job;
class Server : public EventReceiver
{
public:
    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x00,
        NoClangIncludePath = 0x01,
        NoValidate = 0x02,
        ClearProjects = 0x04,
        NoWall = 0x08,
        IgnorePrintfFixits = 0x10
    };
    ThreadPool *threadPool() const { return mIndexerThreadPool; }
    void startQueryJob(const shared_ptr<Job> &job);
    void startIndexerJob(const shared_ptr<IndexerJob> &job, int priority);
    struct Options {
        Options() : options(0), threadCount(0) {}
        Path projectsFile, socketFile, dataDir;
        unsigned options;
        int threadCount;
        List<ByteArray> defaultArguments, excludeFilter;
    };
    bool init(const Options &options);
    const List<ByteArray> &excludeFilter() const { return mOptions.excludeFilter; }
    const Path &clangPath() const { return mClangPath; }
    const Options &options() const { return mOptions; }
private:
    bool selectProject(const ByteArray &pattern, Connection *conn);
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
    void onFileReady(const GccArguments &file, MakefileParser *parser);
    bool processSourceFile(const GccArguments &args, const Path &makefile);
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDestroyed(Connection *o);
    void onMakefileParserDone(MakefileParser *parser);
    void onMakefileModified(const Path &path);
    void onMakefileRemoved(const Path &path);
    bool make(const Path &path, const List<ByteArray> &makefileArgs = List<ByteArray>(),
              const List<ByteArray> &extraCompilerFlags = List<ByteArray>(),
              Connection *conn = 0);
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
    void remake(const ByteArray &pattern = ByteArray(), Connection *conn = 0);
    bool updateProjectForLocation(const Location &location);
    bool updateProjectForLocation(const Path &path);
    void writeProjects();
    struct ProjectEntry;
    bool initSmartProject(const ProjectEntry &entry);
    shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    void removeProject(const Path &key);
    void unloadProject(const Path &key);
    void reloadProjects();
    void onCompletionStreamDisconnected(LocalClient *client);
    bool addProject(const Path &path, const ProjectEntry &entry);
    void onCompletionJobFinished(Path path);
    void startCompletion(const Path &path, int line, int column, const ByteArray &contents, Connection *conn);

    struct ProjectEntry
    {
        ProjectEntry(unsigned t = 0)
            : type(t)
        {}
        unsigned type;
        List<ByteArray> flags, args;
        Path saveKey;
        shared_ptr<Project> project;

        // ### these do not compare saveKey
        inline bool operator==(const ProjectEntry &other) const
        {
            return (type == other.type && flags == other.flags && args == other.args);
        }
        inline bool operator!=(const ProjectEntry &other) const
        {
            return (type != other.type || flags != other.flags || args != other.args);
        }
    };
    typedef Map<Path, ProjectEntry> ProjectsMap;
    ProjectsMap mProjects;
    weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    LocalServer *mServer;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;

    FileSystemWatcher mMakefilesWatcher;
    ThreadPool *mIndexerThreadPool;
    ThreadPool mQueryThreadPool;
    signalslot::Signal2<int, const List<ByteArray> &> mComplete;
    Path mClangPath;

    Map<shared_ptr<Indexer>, int> mSaveTimers;

    Map<LocalClient*, Connection*> mCompletionStreams;
    struct PendingCompletion
    {
        PendingCompletion()
            : line(-1), column(-1), connection(0)
        {}
        int line, column;
        ByteArray contents;
        Connection *connection;
    };
    Map<Path, PendingCompletion> mPendingCompletions;
    Set<Path> mActiveCompletions;

    bool mRestoreProjects;

    enum { DatabaseVersion = 4 };
    friend class CommandProcess;
};

#endif
