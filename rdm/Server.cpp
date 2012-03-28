#include "Server.h"
#include "Indexer.h"
#include "Database.h"
#include "Connection.h"
#include "Resource.h"
#include "Message.h"
#include "Messages.h"
#include "Compressor.h"
#include "QueryMessage.h"
#include "SHA256.h"
#include <QThread>
#include <QThreadPool>
#include <QTcpServer>
#include <QTcpSocket>
#include <stdio.h>
#include <Log.h>

#define ASTPATH "/tmp/rdm"

Server::Server(QObject* parent)
    : QObject(parent),
      mIndexer(new Indexer(ASTPATH, this)),
      mDb(new Database(this)),
      mServer(new QTcpServer(this)),
      mVerbose(false)
{
}

bool Server::init(unsigned options, const QList<QByteArray> &defaultArguments)
{
    mOptions = options;
    mDefaultArgs = defaultArguments;
    Compressor::init();
    Messages::init();
    Resource::setBaseDirectory(ASTPATH);
    Database::setBaseDirectory(ASTPATH);

    if (!mServer->listen(QHostAddress::Any, Connection::Port)) {
        qWarning("Unable to listen to port %d", Connection::Port);
        return false;
    }
    connect(mServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    connect(mIndexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));
    connect(mDb, SIGNAL(complete(int, QList<QByteArray>)),
            this, SLOT(onDatabaseComplete(int, QList<QByteArray>)));
#ifdef CLANG_RUNTIME_INCLUDE
    Path p;
    p = CLANG_RUNTIME_INCLUDE;
    if (p.isDir())
        mDefaultArgs.append("-I" + p);
#endif
    mIndexer->setDefaultArgs(mDefaultArgs);
    log(1) << "running with" << mDefaultArgs;
    return true;
}

void Server::onNewConnection()
{
    while (mServer->hasPendingConnections()) {
        QTcpSocket* socket = mServer->nextPendingConnection();
        Connection* conn = new Connection(socket, this);
        connect(conn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
        connect(socket, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
        connect(conn, SIGNAL(destroyed(QObject*)), this, SLOT(onConnectionDestroyed(QObject*)));
    }
}

void Server::onConnectionDestroyed(QObject* o)
{
    QHash<int, Connection*>::iterator it = mPendingIndexes.begin();
    QHash<int, Connection*>::const_iterator end = mPendingIndexes.end();
    while (it != end) {
        if (it.value() == o)
            mPendingIndexes.erase(it++);
        else
            ++it;
    }

    it = mPendingLookups.begin();
    end = mPendingLookups.end();
    while (it != end) {
        if (it.value() == o)
            mPendingLookups.erase(it++);
        else
            ++it;
    }
}

void Server::onNewMessage(Message* message)
{
    switch (message->messageId()) {
    case AddMessage::MessageId:
        handleAddMessage(static_cast<AddMessage*>(message));
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(static_cast<QueryMessage*>(message));
        break;
    case ErrorMessage::MessageId:
        handleErrorMessage(static_cast<ErrorMessage*>(message));
        break;
    default:
        warning("Unknown message: %d", message->messageId());
        break;
    }

    message->deleteLater();
}

static inline QList<QByteArray> pch(const AddMessage* message)
{
    QList<QByteArray> out;
    foreach(const QByteArray &arg, message->pchs()) {
        if (!arg.isEmpty()) {
            switch (message->type()) {
            case RTags::CompileCPlusPlus:
            case RTags::PchCPlusPlus:
                out.append("-include-pch");
                out.append(arg);
                break;
            default:
                break;
            }
        }
    }
    return out;
}

void Server::handleAddMessage(AddMessage* message)
{
    Connection* conn = qobject_cast<Connection*>(sender());

    int id = mIndexer->index(message->inputFile(), message->arguments() + pch(message));
    mPendingIndexes[id] = conn;
}

void Server::handleQueryMessage(QueryMessage* message)
{
    if (message->query().isEmpty()) {
        return;
    }

    Connection* conn = qobject_cast<Connection*>(sender());
    int id = 0;
    switch (message->type()) {
    case QueryMessage::FollowLocation:
        id = mDb->followLocation(*message);
        break;
    case QueryMessage::ReferencesLocation:
        id = mDb->referencesForLocation(*message);
        break;
    case QueryMessage::ReferencesName:
        id = mDb->referencesForName(*message);
        break;
    case QueryMessage::Recompile:
        id = mDb->recompile(*message);
        break;
    case QueryMessage::ListSymbols:
    case QueryMessage::FindSymbols:
        id = mDb->match(*message);
        break;
    case QueryMessage::Dump:
        id = mDb->dump(*message);
        break;
    case QueryMessage::Status:
        id = mDb->status(*message);
        break;
    case QueryMessage::Poke:
        id = mDb->poke(*message);
        break;
    }
    if (!id) {
        QueryMessage msg(QList<QByteArray>() << "Invalid message", QueryMessage::FollowLocation);
        conn->send(&msg);
    } else {
        mPendingLookups[id] = conn;
    }
}

void Server::handleErrorMessage(ErrorMessage* message)
{
    qWarning("Error message: %s", message->message().constData());
}

void Server::onIndexingDone(int id)
{
    QHash<int, Connection*>::iterator it = mPendingIndexes.find(id);
    if (it == mPendingIndexes.end())
        return;
    ErrorMessage msg("Hello, world");
    it.value()->send(&msg);
}

void Server::onDatabaseComplete(int id, const QList<QByteArray>& response)
{
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    QueryMessage msg(response, QueryMessage::FollowLocation);
    it.value()->send(&msg);
}
