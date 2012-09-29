#ifndef Server_h
#define Server_h

#include <FileSystemWatcher.h>
#include <ByteArray.h>
#include <List.h>
#include <Map.h>
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
struct MakefileInformation;
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
    void startJob(Job *job, int timeout);
    struct Options {
        Options() : options(0), threadCount(0) {}
        Path projectsFile, socketFile;
        unsigned options;
        int threadCount;
        List<ByteArray> defaultArguments, excludeFilter;
    };
    bool init(const Options &options);
    const List<ByteArray> &excludeFilter() const { return mOptions.excludeFilter; }
    const Path &clangPath() const { return mClangPath; }
private:
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
    void test(const QueryMessage &query, Connection *conn);
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

    Map<Path, shared_ptr<Project> > mProjects;
    weak_ptr<Project> mCurrentProject;
    ThreadPool *mThreadPool;
    signalslot::Signal2<int, const List<ByteArray> &> mComplete;
    Path mClangPath;
};

#endif
