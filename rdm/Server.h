#ifndef RDM_H
#define RDM_H

#include <QObject>
#include <QByteArray>
#include <QList>
#include <QHash>
#include "QueryMessage.h"

class Connection;
class Indexer;
class Message;
class AddMessage;
class ErrorMessage;
class QTcpServer;

#ifndef ASTPATH
#define ASTPATH "/tmp/rdm/"
#endif

class Server : public QObject
{
    Q_OBJECT
public:
    Server(QObject *parent = 0);
    enum Option {
        NoOptions = 0x0
    };
    enum DatabaseType {
        General,
        Dependency,
        Symbol,
        SymbolName,
        FileInformation,
        PCH,
        DatabaseTypeCount
    };


    bool init(unsigned options, const QList<QByteArray> &defaultArguments);
    static void setBaseDirectory(const QByteArray& base);
    static QByteArray databaseName(DatabaseType type);
signals:
    void complete(int id, const QList<QByteArray>& locations);
private slots:
    void onNewConnection();
    void onNewMessage(Message* message);
    void onIndexingDone(int id);
    void onComplete(int id, const QList<QByteArray>& response);
    void onConnectionDestroyed(QObject* o);
private:
    void handleAddMessage(AddMessage* message);
    void handleQueryMessage(QueryMessage* message);
    void handleErrorMessage(ErrorMessage* message);
    int followLocation(const QueryMessage &query);
    int referencesForLocation(const QueryMessage &query);
    int referencesForName(const QueryMessage &query);
    int recompile(const QueryMessage &query);
    int match(const QueryMessage &query);
    int dump(const QueryMessage &query);
    int status(const QueryMessage &query);
    int poll(const QueryMessage &query);
    int nextId();
private:
    unsigned mOptions;
    Indexer* mIndexer;
    QTcpServer* mServer;
    QHash<int, Connection*> mPendingIndexes;
    QHash<int, QPair<Connection*, QSet<QByteArray> > > mPendingLookups;
    QList<QByteArray> mDefaultArgs;
    bool mVerbose;
    int mJobId;
    static QByteArray sBase;
};

#endif
