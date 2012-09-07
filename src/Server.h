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
#include "MakefileInformation.h"
#include "GRTags.h"
#include "Project.h"
#include "GRScanJob.h"

class GRTagsMessage;
class Connection;
class Indexer;
class Message;
class ErrorMessage;
class OutputMessage;
class MakefileMessage;
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
        UseDashB = 0x2,
        NoValidate = 0x4,
        ClearProjects = 0x8
    };
    ThreadPool *threadPool() const { return mThreadPool; }
    void startJob(Job *job);
    struct Options {
        Options() : options(0), cacheSizeMB(0), threadCount(0) {}
        Path projectsFile, socketFile;
        unsigned options;
        long cacheSizeMB;
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
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDestroyed(Connection *o);
    void onMakefileParserDone(MakefileParser *parser);
    void onMakefileModified(const Path &path);
    void onMakefileRemoved(const Path &path);
    void make(const Path &path, List<ByteArray> makefileArgs = List<ByteArray>(),
              const List<ByteArray> &extraFlags = List<ByteArray>(), Connection *conn = 0);
    void clearProjects();
    enum InitProjectFlag {
        EnableIndexer = 0x1,
        EnableGRTags = 0x2
    };
    shared_ptr<Project> initProject(const Path &path, unsigned flags);
    void handleMakefileMessage(MakefileMessage *message, Connection *conn);
    void handleGRTagMessage(GRTagsMessage *message, Connection *conn);
    void handleQueryMessage(QueryMessage *message, Connection *conn);
    void handleErrorMessage(ErrorMessage *message, Connection *conn);
    void handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void errors(const QueryMessage &query, Connection *conn);
    int parse(const QueryMessage &query);
    int followLocation(const QueryMessage &query);
    int cursorInfo(const QueryMessage &query);
    int referencesForLocation(const QueryMessage &query);
    int referencesForName(const QueryMessage &query);
    int findSymbols(const QueryMessage &query);
    int listSymbols(const QueryMessage &query);
    int status(const QueryMessage &query);
    int test(const QueryMessage &query);
    int runTest(const QueryMessage &query);
    int findFile(const QueryMessage &query);
    int nextId();
    void reindex(const ByteArray &pattern);
    void remake(const ByteArray &pattern = ByteArray(), Connection *conn = 0);
    ByteArray completions(const QueryMessage &query);
    bool updateProjectForLocation(const Location &location);
    shared_ptr<Project> currentProject() const { return mCurrentProject; }

    static Server *sInstance;
    Options mOptions;
    LocalServer *mServer;
    Map<int, Connection*> mPendingIndexes;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;
    Map<Path, MakefileInformation> mMakefiles;
    FileSystemWatcher mMakefilesWatcher;

    Map<Path, shared_ptr<Project> > mProjects;
    shared_ptr<Project> mCurrentProject;
    ThreadPool *mThreadPool;
    signalslot::Signal2<int, const List<ByteArray> &> mComplete;
    Path mClangPath;
};

#endif
