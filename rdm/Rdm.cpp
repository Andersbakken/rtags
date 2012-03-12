#include "Rdm.h"
#include "Indexer.h"
#include "Database.h"
#include "Connection.h"
#include "Resource.h"
#include "Message.h"
#include "Messages.h"
#include "Compressor.h"
#include "UnitCache.h"
#include "QueryMessage.h"
#include "SHA256.h"
#include <QThread>
#include <QThreadPool>
#include <QTcpServer>
#include <QTcpSocket>
#include <stdio.h>

#define ASTPATH "/tmp/rdm"

Rdm::Rdm(QObject* parent)
    : QObject(parent),
      m_indexer(new Indexer(ASTPATH, this)),
      m_db(new Database(this)),
      m_server(new QTcpServer(this)),
      m_verbose(false)
{
}

bool Rdm::init(const QList<QByteArray> &defaultArguments)
{
    m_defaultArgs = defaultArguments;
    Compressor::init();
    Messages::init();
    UnitCache::instance();
    Resource::setBaseDirectory(ASTPATH);
    Database::setBaseDirectory(ASTPATH);

    if (!m_server->listen(QHostAddress::Any, Connection::Port)) {
        qWarning("Unable to listen to port %d", Connection::Port);
        return false;
    }
    connect(m_server, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    connect(m_indexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));
    connect(m_db, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SLOT(onDatabaseComplete(int, const QList<QByteArray>&)));
    return true;
}

void Rdm::onNewConnection()
{
    while (m_server->hasPendingConnections()) {
        QTcpSocket* socket = m_server->nextPendingConnection();
        Connection* conn = new Connection(socket, this);
        connect(conn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
        connect(socket, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
        connect(conn, SIGNAL(destroyed(QObject*)), this, SLOT(onConnectionDestroyed(QObject*)));
    }
}

void Rdm::onConnectionDestroyed(QObject* o)
{
    QHash<int, Connection*>::iterator it = m_pendingIndexes.begin();
    QHash<int, Connection*>::const_iterator end = m_pendingIndexes.end();
    while (it != end) {
        if (it.value() == o)
            m_pendingIndexes.erase(it++);
        else
            ++it;
    }

    it = m_pendingLookups.begin();
    end = m_pendingLookups.end();
    while (it != end) {
        if (it.value() == o)
            m_pendingLookups.erase(it++);
        else
            ++it;
    }
}

void Rdm::onNewMessage(Message* message)
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
        qWarning("Unknown message: %d", message->messageId());
        break;
    }

    message->deleteLater();
}

static inline QList<QByteArray> pch(const AddMessage* message)
{
    QList<QByteArray> out;
    foreach(QByteArray arg, message->pchs()) {
        if (arg.isEmpty())
            continue;
        out.append("-include-pch");
        if (message->type() == AddMessage::CompileC
            || message->type() == AddMessage::PchC)
            arg += "/pch-c";
        else
            arg += "/pch-c++";
        out.append(arg);
    }
    return out;
}

void Rdm::handleAddMessage(AddMessage* message)
{
    Connection* conn = qobject_cast<Connection*>(sender());

    QByteArray outputfile = message->outputFile();
    Indexer::Type type;
    switch (message->type()) {
    case AddMessage::PchC:
        outputfile += "/pch-c";
        // fall through
    case AddMessage::CompileC:
        type = Indexer::C;
        break;
    case AddMessage::PchCPlusPlus:
        outputfile += "/pch-c++";
        // fall through
    case AddMessage::CompileCPlusPlus:
        type = Indexer::CPlusPlus;
        break;
    default:
        qWarning("%s: Invalid type %d\n", __FUNCTION__, message->type());
        return;
    }
    int id = m_indexer->index(type, message->inputFile(), outputfile,
                              message->arguments() + m_defaultArgs + pch(message),
                              Indexer::Force);
    m_pendingIndexes[id] = conn;
}

void Rdm::handleQueryMessage(QueryMessage* message)
{
    if (message->query().isEmpty()) {
        return;
    }

    Connection* conn = qobject_cast<Connection*>(sender());
    int id = 0;
    switch(message->type()) {
    case QueryMessage::FollowLocation:
        id = m_db->followLocation(*message);
        break;
    case QueryMessage::CursorInfo:
        id = m_db->cursorInfo(*message);
        break;
    case QueryMessage::CodeComplete:
        id = m_db->codeComplete(*message);
        break;
    case QueryMessage::ReferencesLocation:
        id = m_db->referencesForLocation(*message);
        break;
    case QueryMessage::ReferencesName:
        id = m_db->referencesForName(*message);
        break;
    case QueryMessage::Recompile:
        id = m_db->recompile(*message);
        break;
    case QueryMessage::Match:
        id = m_db->match(*message);
        break;
    case QueryMessage::Dump:
        id = m_db->dump(*message);
        break;
    default:
        qWarning("Unknown message type %d\n", message->type());
        return;
    }
    m_pendingLookups[id] = conn;
}

void Rdm::handleErrorMessage(ErrorMessage* message)
{
    qWarning("Error message: %s", message->message().constData());
}

void Rdm::onIndexingDone(int id)
{
    QHash<int, Connection*>::iterator it = m_pendingIndexes.find(id);
    if (it == m_pendingIndexes.end())
        return;
    ErrorMessage msg("Hello, world");
    it.value()->send(&msg);
}

void Rdm::onDatabaseComplete(int id, const QList<QByteArray>& response)
{
    QHash<int, Connection*>::iterator it = m_pendingLookups.find(id);
    if (it == m_pendingLookups.end())
        return;
    QueryMessage msg(response, QueryMessage::FollowLocation);
    it.value()->send(&msg);
}
