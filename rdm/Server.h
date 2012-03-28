#ifndef RDM_H
#define RDM_H

#include <QObject>
#include <QByteArray>
#include <QList>
#include <QHash>

class Connection;
class Indexer;
class Database;
class Message;
class AddMessage;
class QueryMessage;
class ErrorMessage;
class QTcpServer;

class Server : public QObject
{
    Q_OBJECT
public:
    Server(QObject *parent = 0);
    enum Option {
        NoOptions = 0x0
    };
    bool init(unsigned options, const QList<QByteArray> &defaultArguments);
private slots:
    void onNewConnection();
    void onNewMessage(Message* message);
    void onIndexingDone(int id);
    void onDatabaseComplete(int id, const QList<QByteArray>& response);
    void onConnectionDestroyed(QObject* o);

private:
    void handleAddMessage(AddMessage* message);
    void handleQueryMessage(QueryMessage* message);
    void handleErrorMessage(ErrorMessage* message);

private:
    unsigned mOptions;
    Indexer* mIndexer;
    Database* mDb;
    QTcpServer* mServer;
    QHash<int, Connection*> mPendingIndexes;
    QHash<int, Connection*> mPendingLookups;
    QList<QByteArray> mDefaultArgs;
    bool mVerbose;
};

#endif
