#include "Rdm.h"
#include "Indexer.h"
#include "Database.h"
#include "Connection.h"
#include "Resource.h"
#include "Message.h"
#include "Messages.h"
#include "Compressor.h"
#include <QThread>
#include <QThreadPool>
#include <QTcpServer>
#include <QTcpSocket>
#include <stdio.h>

Rdm::Rdm(int& argc, char**& argv, QObject* parent)
    : QObject(parent),
      m_indexer(new Indexer("/tmp/rdm", this)),
      m_db(new Database(this)),
      m_server(new QTcpServer(this))
{
    Compressor::init();
    Messages::init();
    Resource::setBaseDirectory("/tmp/rdm");
    Database::setBaseDirectory("/tmp/rdm");

    int jobs = QThread::idealThreadCount();

    for (int i = 0; i < argc; ++i) {
        if (!strncmp(argv[i], "-I", 2))
            m_defaultArgs.append(argv[i]);
        else if (!strncmp(argv[i], "-j", 2)) {
            QByteArray ba(argv[i] + 2);
            bool ok;
            int newjobs = ba.toInt(&ok);
            if (ok)
                jobs = newjobs;
        }
    }

    QThreadPool::globalInstance()->setMaxThreadCount(jobs);
    printf("Running with %d jobs\n", jobs);

    if (!m_server->listen(QHostAddress::Any, Connection::Port))
        qFatal("Unable to listen to port %d", Connection::Port);
    connect(m_server, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    connect(m_indexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));
    connect(m_db, SIGNAL(complete(int, const QList<QByteArray>&)),
            this, SLOT(onDatabaseComplete(int, const QList<QByteArray>&)));
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
    qDebug() << "got message" << message->messageId();
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

void Rdm::handleAddMessage(AddMessage* message)
{
    Connection* conn = qobject_cast<Connection*>(sender());
    int id = m_indexer->index(message->inputFile(), message->arguments() + m_defaultArgs, Indexer::Force);
    m_pendingIndexes[id] = conn;
}

void Rdm::handleQueryMessage(QueryMessage* message)
{
    if (message->query().isEmpty())
        return;

    Connection* conn = qobject_cast<Connection*>(sender());
    int id = 0;
    switch(message->type()) {
    case QueryMessage::FollowLocation:
        id = m_db->followLocation(message->query().front());
        break;
    case QueryMessage::ReferencesLocation:
        id = m_db->referencesForLocation(message->query().front());
        break;
    case QueryMessage::ReferencesName:
        id = m_db->referencesForName(message->query().front());
        break;
    case QueryMessage::Recompile:
        id = m_db->recompile(message->query().front());
        break;
    case QueryMessage::Match:
        id = m_db->match(message->query().front());
        break;
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
