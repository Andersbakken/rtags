#ifndef Server_h
#define Server_h

#include <QObject>
#include <ByteArray.h>
#include <QList>
#include <Hash.h>
#include "QueryMessage.h"
#include "Connection.h"
#include "Job.h"

class Connection;
class Indexer;
class Message;
class ErrorMessage;
class OutputMessage;
class MakefileMessage;
class QLocalServer;
class Database;
class GccArguments;
class ScopedDB
{
public:
    enum LockType {
        Read,
        Write
    };
    ScopedDB(Database *db, LockType lockType);
    Database *operator->() { return mData->db; }
    operator Database *() { return mData->db; }
private:
    class Data : public QSharedData
    {
    public:
        Data(Database *database, LockType lockType);
        ~Data();
        Database *db;
        LockType lock;
    };
    QSharedDataPointer<Data> mData;
};

class Server : public QObject
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
        PCHUsrHashes,
        FileIds,
        DatabaseTypeCount
    };

    static Server *instance() { return sInstance; }
    QList<ByteArray> defaultArguments() const { return mOptions.defaultArguments; }
    inline ScopedDB db(DatabaseType type, ScopedDB::LockType lockType) const { return ScopedDB(mDBs[type], lockType); }
    struct Options {
        Options() : options(0), cacheSizeMB(0) {}
        unsigned options;
        QList<ByteArray> defaultArguments;
        long cacheSizeMB;
        ByteArray name;
    };
    bool init(const Options &options);
    Indexer *indexer() const { return mIndexer; }
    ByteArray name() const { return mOptions.name; }
    static void setBaseDirectory(const ByteArray &base, bool clear);
    static Path databaseDir(DatabaseType type);
    static Path pchDir();
    QThreadPool *threadPool() const { return mThreadPool; }
signals:
    void complete(int id, const QList<ByteArray> &locations);
private slots:
    void onFileReady(const GccArguments &file);
    void onNewConnection();
    void onNewMessage(Message *message);
    void onIndexingDone(int id);
    void onComplete(int id);
    void onOutput(int id, const ByteArray &response);
    void onConnectionDestroyed(QObject *o);
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
    int dump(const QueryMessage &query);
    int status(const QueryMessage &query);
    int test(const QueryMessage &query);
    int runTest(const QueryMessage &query);
    int nextId();
    void startJob(Job *job);
    void reindex(const ByteArray &pattern);
    void remake(const ByteArray &pattern, Connection *conn);
    void rdmLog(const QueryMessage &message, Connection *conn);
    void make(const Path &path, QList<ByteArray> makefileArgs, const QList<ByteArray> &extraFlags);
private:
    static Server *sInstance;
    Options mOptions;
    Indexer *mIndexer;
    QLocalServer *mServer;
    Hash<int, Connection*> mPendingIndexes;
    Hash<int, Connection*> mPendingLookups;
    bool mVerbose;
    int mJobId;
    static Path sBase;
    Hash<Path, QPair<QList<ByteArray>, QList<ByteArray> > > mMakefiles;
    Database *mDBs[DatabaseTypeCount];
    QThreadPool *mThreadPool;
};

#endif
