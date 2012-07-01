#ifndef Server_h
#define Server_h

#include <FileSystemWatcher.h>
#include <ByteArray.h>
#include <List.h>
#include <Map.h>
#include "QueryMessage.h"
#include "Connection.h"
#include "ThreadPool.h"
#include "Job.h"
#include "Rdm.h"
#include "ScopedDB.h"
#include "EventReceiver.h"

class Connection;
class Indexer;
class Message;
class ErrorMessage;
class OutputMessage;
class MakefileMessage;
class LocalServer;
class Database;
class GccArguments;
class MakefileParser;
class Completions;
class Server : public EventReceiver
{
public:
    Server();
    ~Server();
    void clear();
    enum Option {
        NoOptions = 0x0,
        NoClangIncludePath = 0x1,
        UseDashB = 0x2
    };
    enum DatabaseType {
        General,
        Dependency,
        Symbol,
        SymbolName,
        FileInformation,
        PCHUsrMaps,
        FileIds,
        DatabaseTypeCount
    };

    static Server *instance() { return sInstance; }
    List<ByteArray> defaultArguments() const { return mOptions.defaultArguments; }
    inline ScopedDB db(DatabaseType type, ScopedDB::LockType lockType) const { return ScopedDB(mDBs[type], lockType); }
    struct Options {
        Options() : options(0), cacheSizeMB(0), maxCompletionUnits(0) {}
        unsigned options;
        List<ByteArray> defaultArguments;
        long cacheSizeMB;
        ByteArray name;
        int maxCompletionUnits;
    };
    bool init(const Options &options);
    Indexer *indexer() const { return mIndexer; }
    ByteArray name() const { return mOptions.name; }
    static bool setBaseDirectory(const ByteArray &base, bool clear);
    static Path databaseDir(DatabaseType type);
    static Path pchDir();
    ThreadPool *threadPool() const { return mThreadPool; }
    void onNewConnection();
    signalslot::Signal2<int, const List<ByteArray> &> &complete() { return mComplete; }
    void startJob(Job *job);
protected:
    void event(const Event *event);
    void onFileReady(const GccArguments &file, MakefileParser *parser);
    void onNewMessage(Message *message, Connection *conn);
    void onIndexingDone(int id);
    void onConnectionDestroyed(Connection *o);
    void onMakefileParserDone(MakefileParser *parser);
    void onMakefileModified(const Path &path);
    void onMakefileRemoved(const Path &path);
    void make(const Path &path, List<ByteArray> makefileArgs = List<ByteArray>(),
              const List<ByteArray> &extraFlags = List<ByteArray>(), Connection *conn = 0);
private:
    void handleMakefileMessage(MakefileMessage *message, Connection *conn);
    void handleQueryMessage(QueryMessage *message, Connection *conn);
    void handleErrorMessage(ErrorMessage *message, Connection *conn);
    void handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void errors(const QueryMessage &query, Connection *conn);
    int followLocation(const QueryMessage &query);
    int cursorInfo(const QueryMessage &query);
    int referencesForLocation(const QueryMessage &query);
    int referencesForName(const QueryMessage &query);
    int findSymbols(const QueryMessage &query);
    int listSymbols(const QueryMessage &query);
    int status(const QueryMessage &query);
    int test(const QueryMessage &query);
    int runTest(const QueryMessage &query);
    int nextId();
    void reindex(const ByteArray &pattern);
    void remake(const ByteArray &pattern = ByteArray(), Connection *conn = 0);
    ByteArray completions(const QueryMessage &query);
private:
    static Server *sInstance;
    Options mOptions;
    Indexer *mIndexer;
    LocalServer *mServer;
    Map<int, Connection*> mPendingIndexes;
    Map<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;
    static Path sBase;
    Map<Path, MakefileInformation> mMakefiles;
    FileSystemWatcher mMakefilesWatcher;
    Database *mDBs[DatabaseTypeCount];
    ThreadPool *mThreadPool;
    signalslot::Signal2<int, const List<ByteArray> &> mComplete;
    Completions *mCompletions;
};

#endif
