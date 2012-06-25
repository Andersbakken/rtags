#ifndef Server_h
#define Server_h

#include <QObject>
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
class Server : public QObject, public EventReceiver
{
    Q_OBJECT
public:
    Server(QObject *parent = 0);
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
        PCHUsrMapes,
        FileIds,
        DatabaseTypeCount
    };

    static Server *instance() { return sInstance; }
    List<ByteArray> defaultArguments() const { return mOptions.defaultArguments; }
    inline ScopedDB db(DatabaseType type, ScopedDB::LockType lockType) const { return ScopedDB(mDBs[type], lockType); }
    struct Options {
        Options() : options(0), cacheSizeMB(0) {}
        unsigned options;
        List<ByteArray> defaultArguments;
        long cacheSizeMB;
        ByteArray name;
    };
    bool init(const Options &options);
    Indexer *indexer() const { return mIndexer; }
    ByteArray name() const { return mOptions.name; }
    static void setBaseDirectory(const ByteArray &base, bool clear);
    static Path databaseDir(DatabaseType type);
    static Path pchDir();
    ThreadPool *threadPool() const { return mThreadPool; }
    void onNewConnection();
protected:
    void event(const Event *event);
signals:
    void complete(int id, const List<ByteArray> &locations);
private slots:
    void onFileReady(const GccArguments &file);
    void onNewMessage(Message *message);
    void onIndexingDone(int id);
    void onConnectionDestroyed(QObject *o);
    void onMakefileParserDone(int sourceCount, int pchCount);
    void onMakefileModified(const Path &path);
    void onMakefileRemoved(const Path &path);
private:
    void handleMakefileMessage(MakefileMessage *message);
    void handleQueryMessage(QueryMessage *message);
    void handleErrorMessage(ErrorMessage *message);
    void handleOutputMessage(OutputMessage *message);
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
    void startJob(Job *job);
    void reindex(const ByteArray &pattern);
    void remake(const ByteArray &pattern = ByteArray(), Connection *conn = 0);
    void rdmLog(const QueryMessage &message, Connection *conn);
    void make(const Path &path, List<ByteArray> makefileArgs, const List<ByteArray> &extraFlags);
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
};

#endif
