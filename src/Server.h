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
#include "MakefileInformation.h"

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
        NoOptions = 0x0,
        NoClangIncludePath = 0x1,
        NoValidate = 0x2,
        ClearProjects = 0x4,
        NoWall = 0x8
    };
    ThreadPool *threadPool() const { return mThreadPool; }
    void startJob(const shared_ptr<Job> &job);
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
private:
    void onJobsComplete(shared_ptr<Indexer> indexer, int count);
    void onJobStarted(shared_ptr<Indexer> indexer, Path path);

    static void saveTimerCallback(int id, void *userData);
    void save(const shared_ptr<Indexer> &indexer);
    void restore();
    void clear();
    void onNewConnection();
    signalslot::Signal2<int, const List<ByteArray> &> &complete() { return mComplete; }
    shared_ptr<Project> setCurrentProject(const Path &path);
    shared_ptr<Project> setCurrentProject(const shared_ptr<Project> &proj);
    void event(const Event *event);
    void onFileReady(const GccArguments &file, MakefileParser *parser);
    bool processSourceFile(const GccArguments &args, const Path &makefile);
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDestroyed(Connection *o);
    void onMakefileParserDone(MakefileParser *parser);
    void onMakefileModified(const Path &path);
    void onMakefileRemoved(const Path &path);
    void make(const Path &path, const List<ByteArray> &makefileArgs = List<ByteArray>(),
              const List<ByteArray> &extraCompilerFlags = List<ByteArray>(),
              Connection *conn = 0);
    void clearProjects();
    void handleProjectMessage(ProjectMessage *message, Connection *conn);
    void handleQueryMessage(QueryMessage *message, Connection *conn);
    void handleErrorMessage(ErrorMessage *message, Connection *conn);
    void handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void errors(const QueryMessage &query, Connection *conn);
    void followLocation(const QueryMessage &query, Connection *conn);
    void cursorInfo(const QueryMessage &query, Connection *conn);
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
    void deleteProject(const QueryMessage &query, Connection *conn);
    void project(const QueryMessage &query, Connection *conn);
    void clearProjects(const QueryMessage &query, Connection *conn);
    void shutdown(const QueryMessage &query, Connection *conn);
    int nextId();
    void reindex(const QueryMessage &query, Connection *conn);
    void remake(const ByteArray &pattern = ByteArray(), Connection *conn = 0);
    ByteArray completions(const QueryMessage &query);
    bool updateProjectForLocation(const Location &location);
    bool updateProjectForLocation(const Path &path, Path *key = 0);
    void writeProjects();
    bool grtag(const Path &dir);
    bool smartProject(const Path &path, const List<ByteArray> &extraCompilerFlags);
    shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    void removeProject(const Path &key);

    static Server *sInstance;
    Options mOptions;
    LocalServer *mServer;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;
    Map<Path, MakefileInformation> mMakefiles;
    Set<Path> mGRTagsDirs;
    Map<Path, List<ByteArray> > mSmartProjects;
    FileSystemWatcher mMakefilesWatcher;

    ProjectsMap mProjects;
    weak_ptr<Project> mCurrentProject;
    ThreadPool *mThreadPool;
    signalslot::Signal2<int, const List<ByteArray> &> mComplete;
    Path mClangPath;

    Map<shared_ptr<Indexer>, int> mSaveTimers;

    enum { DatabaseVersion = 2 };
};

#endif
