#ifndef Server_h
#define Server_h

#include <QObject>
#include <QByteArray>
#include <QList>
#include <QHash>
#include "QueryMessage.h"
#include "Connection.h"
#include "Job.h"

class Connection;
class Indexer;
class Message;
class AddMessage;
class ErrorMessage;
class QLocalServer;
class Database;
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
    enum Option {
        NoOptions = 0x0,
        NoClangIncludePath = 0x1
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
    QList<QByteArray> defaultArguments() const { return mDefaultArgs; }
    inline ScopedDB db(DatabaseType type, ScopedDB::LockType lockType) const { return ScopedDB(mDBs[type], lockType); }
    struct Options {
        unsigned options;
        QList<QByteArray> defaultArguments;
        long cacheSizeMB;
        QByteArray name;
    };
    bool init(const Options &options);
    Indexer *indexer() const { return mIndexer; }
    QByteArray name() const { return mName; }
    static void setBaseDirectory(const QByteArray& base, bool clear);
    static Path databaseDir(DatabaseType type);
    static Path pchDir();
signals:
    void complete(int id, const QList<QByteArray>& locations);
private slots:
    void onNewConnection();
    void onNewMessage(Message* message);
    void onIndexingDone(int id);
    void onComplete(int id);
    void onOutput(int id, const QByteArray &response);
    void onConnectionDestroyed(QObject* o);
private:
    void handleAddMessage(AddMessage* message);
    void handleQueryMessage(QueryMessage* message);
    void handleErrorMessage(ErrorMessage* message);
    int followLocation(const QueryMessage &query);
    int cursorInfo(const QueryMessage &query);
    int referencesForLocation(const QueryMessage &query);
    int referencesForName(const QueryMessage &query);
    int match(const QueryMessage &query);
    int dump(const QueryMessage &query);
    int status(const QueryMessage &query);
    int test(const QueryMessage &query);
    int nextId();
    void startJob(Job *job);
    void rdmLog(const QueryMessage &message, Connection *conn);
private:
    static Server *sInstance;
    unsigned mOptions;
    Indexer* mIndexer;
    QLocalServer* mServer;
    QHash<int, Connection*> mPendingIndexes;
    QHash<int, Connection*> mPendingLookups;
    QList<QByteArray> mDefaultArgs;
    bool mVerbose;
    int mJobId;
    QByteArray mName;
    QList<QByteArray> mCachedSymbolNames;
    static Path sBase;
    Database *mDBs[DatabaseTypeCount];
};

#endif
