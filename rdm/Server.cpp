#include "Connection.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "LevelDB.h"
#include "MatchJob.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "TestJob.h"
#include "PollJob.h"
#include "QueryMessage.h"
#include "Rdm.h"
#include "ReferencesJob.h"
#include "SHA256.h"
#include "Server.h"
#include "StatusJob.h"
#include "leveldb/db.h"
#include <Log.h>
#include <QtCore>
#include <QtNetwork>
#include <clang-c/Index.h>
#include <stdio.h>

QByteArray Server::sBase;
Q_DECLARE_METATYPE(QList<QByteArray>);

Server::Server(QObject* parent)
    : QObject(parent),
      mIndexer(0),
      mServer(0),
      mVerbose(false),
      mJobId(0)
{
    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
}

bool Server::init(unsigned options, const QList<QByteArray> &defaultArguments)
{
    mOptions = options;
    mDefaultArgs = defaultArguments;
    Messages::init();
    setBaseDirectory(ASTPATH);
    mServer = new QTcpServer(this);
    mIndexer = new Indexer(ASTPATH, this);

    if (!mServer->listen(QHostAddress::Any, Connection::Port)) {
        error("Unable to listen to port %d", Connection::Port);
        return false;
    }
    connect(mServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    connect(mIndexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));
    connect(this, SIGNAL(complete(int, QList<QByteArray>)),
            this, SLOT(onComplete(int, QList<QByteArray>)));
#ifdef CLANG_RUNTIME_INCLUDE
    Path p;
    p = CLANG_RUNTIME_INCLUDE;
    if (p.isDir())
        mDefaultArgs.append("-I" + p);
#endif
    mIndexer->setDefaultArgs(mDefaultArgs);
    LevelDB db;
    if (db.open(Server::General, LevelDB::ReadOnly)) {
        bool ok;
        const int version = Rdm::readValue<int>(db.db(), "version", &ok);
        if (!ok) {
            error("No version in database");
            return false;
        }
        if (version != Rdm::DatabaseVersion) {
            error("Wrong version, expected %d, got %d. Run with -C to regenerate database", version, Rdm::DatabaseVersion);
            return false;
        }
    } else {
        QByteArray err;
        if (!db.open(Server::General, LevelDB::ReadWrite, &err)) {
            error("Can't open database %s", err.constData());
            return false;
        }
        Rdm::writeValue<int>(db.db(), "version", Rdm::DatabaseVersion);
    }

    debug() << "running with" << mDefaultArgs;
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
    {
        QHash<int, Connection*>::iterator it = mPendingIndexes.begin();
        const QHash<int, Connection*>::const_iterator end = mPendingIndexes.end();
        while (it != end) {
            if (it.value() == o) {
                it = mPendingIndexes.erase(it);
            } else {
                ++it;
            }
        }
    }
    {
        QHash<int, Connection*>::iterator it = mPendingLookups.begin();
        const QHash<int, Connection*>::const_iterator end = mPendingLookups.end();
        while (it != end) {
            if (it.value() == o) {
                it = mPendingLookups.erase(it);
            } else {
                ++it;
            }
        }
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
    foreach (const QByteArray &arg, message->pchs()) {
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
    case QueryMessage::Response:
        Q_ASSERT(0);
        break;
    case QueryMessage::FollowLocation:
        id = followLocation(*message);
        break;
    case QueryMessage::ReferencesLocation:
        id = referencesForLocation(*message);
        break;
    case QueryMessage::ReferencesName:
        id = referencesForName(*message);
        break;
    case QueryMessage::ListSymbols:
    case QueryMessage::FindSymbols:
        id = match(*message);
        break;
    case QueryMessage::Dump:
        id = dump(*message);
        break;
    case QueryMessage::Status:
        id = status(*message);
        break;
    case QueryMessage::Poll:
        id = poll(this);
        break;
    case QueryMessage::Test:
        id = test(*message);
        break;
    }
    if (!id) {
        QueryMessage msg(QList<QByteArray>() << "Invalid message");
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

void Server::onComplete(int id)
{
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    it.value()->finish();
}


void Server::onComplete(int id, const QList<QByteArray>& response)
{
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;

    QueryMessage msg(response);
    it.value()->send(&msg);
    it.value()->finish();
}

void Server::onOutput(int id, const QList<QByteArray> &response)
{
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;

    QueryMessage msg(response);
    it.value()->send(&msg);
}

void Server::onComplete(int id, const QByteArray& response)
{
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    QueryMessage msg(response);
    it.value()->send(&msg);
    it.value()->finish();
}
void Server::onOutput(int id, const QByteArray &response)
{
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    QueryMessage msg(response);
    it.value()->send(&msg);
}


int Server::nextId()
{
    ++mJobId;
    if (!mJobId)
        ++mJobId;
    return mJobId;
}

int Server::followLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc)) {
        error("Failed to make location from [%s]", query.query().front().constData());
        return 0;
    }

    const int id = nextId();

    warning() << "followLocation" << loc;

    FollowLocationJob* job = new FollowLocationJob(id, loc, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::referencesForLocation(const QueryMessage &query)
{
    RTags::Location loc;
    if (!RTags::makeLocation(query.query().front(), &loc))
        return 0;

    const int id = nextId();

    warning() << "references for location" << loc;

    ReferencesJob* job = new ReferencesJob(id, loc, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::referencesForName(const QueryMessage& query)
{
    const int id = nextId();

    const QByteArray name = query.query().front();
    warning() << "references for name" << name;

    ReferencesJob* job = new ReferencesJob(id, name, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::match(const QueryMessage &query)
{
    const QByteArray partial = query.query().front();
    const int id = nextId();

    warning() << "match" << partial;

    MatchJob* job = new MatchJob(id, query);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::dump(const QueryMessage &query)
{
    const QByteArray partial = query.query().front();
    const int id = nextId();

    warning() << "dump" << partial;

    DumpJob* job = new DumpJob(partial, id);
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::status(const QueryMessage &query)
{
    const int id = nextId();

    warning() << "status";

    StatusJob* job = new StatusJob(id, query.query().first());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);
    return id;
}

int Server::poll(const QueryMessage &query)
{
    const int id = nextId();

    warning() << "poll";
    // ### this needs to be implemented

    PollJob *job = new PollJob(mIndexer, id);
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);
    return id;
}

int Server::test(const QueryMessage &query)
{
    const int id = nextId();

    warning() << "poll";

    TestJob *job = new TestJob(query.query().first(), id);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);
    return id;
}

static const char* const dbNames[] = {
    "general.db",
    "dependencies.db",
    "symbols.db",
    "symbolnames.db",
    "fileinfos.db",
    "pch.db"
};

QByteArray Server::databaseName(DatabaseType type)
{
    if (sBase.isEmpty())
        return QByteArray();
    return sBase + dbNames[type];
}

void Server::setBaseDirectory(const QByteArray& base)
{
    sBase = base;
    Q_ASSERT(sBase.endsWith('/'));
}

void Server::connectJob(Job *job)
{
    connect(job, SIGNAL(complete(int)), this, SLOT(onComplete(int)));
    connect(job, SIGNAL(output(int, QByteArray)), this, SLOT(onOutput(int, QByteArray)));
}
