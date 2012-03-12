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

class Rdm : public QObject
{
    Q_OBJECT
public:
    Rdm(QObject *parent = 0);
    bool init();
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
    Indexer* m_indexer;
    Database* m_db;
    QTcpServer* m_server;
    QHash<int, Connection*> m_pendingIndexes;
    QHash<int, Connection*> m_pendingLookups;
    QList<QByteArray> m_defaultArgs;
    bool m_verbose;
};

#endif
