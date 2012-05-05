#include "Connection.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "CursorInfoJob.h"
#include "MatchJob.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "IndexerSyncer.h"
#include "TestJob.h"
#include "QueryMessage.h"
#include "Rdm.h"
#include "ReferencesJob.h"
#include "SHA256.h"
#include "Server.h"
#include "StatusJob.h"
#include "leveldb/db.h"
#include "leveldb/cache.h"
#include <Log.h>
#include <QtCore>
#include <QtNetwork>
#include <clang-c/Index.h>
#include <stdio.h>

Path Server::sBase;
Q_DECLARE_METATYPE(QList<QByteArray>);

Server *Server::sInstance = 0;
Server::Server(QObject* parent)
    : QObject(parent),
      mIndexer(0),
      mServer(0),
      mVerbose(false),
      mJobId(0)
{
    Q_ASSERT(!sInstance);
    sInstance = this;
    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
    memset(mDBs, 0, sizeof(mDBs));
}

Server::~Server()
{
    for (int i=0; i<DatabaseTypeCount; ++i) {
        delete mDBs[i];
    }

    Q_ASSERT(sInstance = this);
    sInstance = 0;
}

bool Server::init(const Options &options)
{
    mOptions = options.options;
    {
        leveldb::Options opt;
        opt.create_if_missing = true;
        opt.block_cache = leveldb::NewLRUCache(options.cacheSizeMB * 1024 * 1024);
        leveldb::Status status;
        for (int i=0; i<DatabaseTypeCount; ++i) {
            status = leveldb::DB::Open(opt, databaseDir(static_cast<DatabaseType>(i)).constData(), &mDBs[i]);
            if (!status.ok()) {
                error() << "Failed to open db" << status.ToString().c_str();
                return false;
            }
        }
    }
    leveldb::DB *general = db(Server::General);
    bool ok;
    const int version = Rdm::readValue<int>(general, "version", &ok);
    if (!ok) {
        Rdm::writeValue<int>(general, "version", Rdm::DatabaseVersion);
    } else if (version != Rdm::DatabaseVersion) {
        error("Wrong version, expected %d, got %d. Run with -C to regenerate database", version, Rdm::DatabaseVersion);
        return false;
    }

    mDefaultArgs = options.defaultArguments;
    Messages::init();
    mServer = new QTcpServer(this);
    mIndexer = new Indexer(sBase, this);
    connect(mIndexer->syncer(), SIGNAL(symbolNamesChanged()), this, SLOT(onSymbolNamesChanged()));

    if (!mServer->listen(QHostAddress::Any, Connection::Port)) {
        error("Unable to listen to port %d", Connection::Port);
        return false;
    }
    connect(mServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    connect(mIndexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));
    QList<Path> systemPaths;
    foreach(const QByteArray &a, mDefaultArgs) {
        if (a.startsWith("-I")) {
            const Path p = Path::resolved(a.constData() + 2);
            if (p.isDir())
                systemPaths.append(p);
        }
    }
#ifdef CLANG_RUNTIME_INCLUDE
    const Path p = Path::resolved(CLANG_RUNTIME_INCLUDE);
    if (p.isDir()) {
        systemPaths.append(p);
        mDefaultArgs.append("-I" + p);
    }
#endif
    Rdm::initSystemPaths(systemPaths);
    mIndexer->setDefaultArgs(mDefaultArgs);

    warning() << "running with" << mDefaultArgs;
    onSymbolNamesChanged();
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

    leveldb::DB *database = db(Server::FileInformation);
    const QList<QByteArray> args = message->arguments() + pch(message);
    if (args != mIndexer->compileArgs(message->inputFile())) {
        const int id = mIndexer->index(message->inputFile(), args);
        if (id != -1)
            mPendingIndexes[id] = conn;
    }
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
    case QueryMessage::CursorInfo:
        id = cursorInfo(*message);
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
        if (message->query().front().isEmpty() && !message->flags() && !mCachedSymbolNames.isEmpty()) {
            QueryMessage response(mCachedSymbolNames);
            conn->send(&response);
            conn->finish();
            return;
        }
        // fall through
    case QueryMessage::FindSymbols:
        id = match(*message);
        break;
    case QueryMessage::Dump:
        id = dump(*message);
        break;
    case QueryMessage::Status:
        id = status(*message);
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
    if (id == MatchJob::CompletionMatchJobId)
        return;
    QHash<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    it.value()->finish();
}

void Server::onOutput(int id, const QByteArray &response)
{
    if (id == MatchJob::CompletionMatchJobId) {
        mCachedSymbolNames.append(response);
        return;
    }
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
    const Location loc = Location::fromKey(query.query().front());
    if (loc.isNull()) {
        error("Failed to make location from [%s]", query.query().front().constData());
        return 0;
    }

    const int id = nextId();

    error() << "followLocation" << loc;

    FollowLocationJob* job = new FollowLocationJob(id, loc, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::cursorInfo(const QueryMessage &query)
{
    const Location loc = Location::fromKey(query.query().front());
    if (loc.isNull()) {
        error("Failed to make location from [%s]", query.query().front().constData());
        return 0;
    }

    const int id = nextId();

    error() << "cursorInfo" << loc;

    CursorInfoJob* job = new CursorInfoJob(id, loc, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}


int Server::referencesForLocation(const QueryMessage &query)
{
    const Location loc = Location::fromKey(query.query().front());
    if (loc.isNull()) {
        error("Failed to make location from [%s]", query.query().front().constData());
        return 0;
    }

    const int id = nextId();

    error() << "references for location" << loc;

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
    error() << "references for name" << name;

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

    error() << "match" << partial;

    MatchJob* job = new MatchJob(id, query);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::dump(const QueryMessage &query)
{
    const QByteArray partial = query.query().front();
    const int id = nextId();

    error() << "dump" << partial;

    DumpJob* job = new DumpJob(partial, id);
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);

    return id;
}

int Server::status(const QueryMessage &query)
{
    const int id = nextId();

    error() << "status" << query.query().front();

    StatusJob* job = new StatusJob(id, query.query().front());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    connectJob(job);
    QThreadPool::globalInstance()->start(job);
    return id;
}

int Server::test(const QueryMessage &query)
{
    const int id = nextId();

    error() << "test";

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
    "pch.db",
    0
};

Path Server::databaseDir(DatabaseType type)
{
    if (sBase.isEmpty())
        return Path();
    return sBase + dbNames[type];
}

Path Server::pchDir()
{
    if (sBase.isEmpty())
        return Path();
    return sBase + "pch";
}

void Server::setBaseDirectory(const QByteArray& base, bool clear)
{
    sBase = base;
    if (!sBase.endsWith('/'))
        sBase.append('/');
    Q_ASSERT(sBase.endsWith('/'));
    QDir dir;
    dir.mkpath(sBase);
    if (clear) {
        RTags::removeDirectory(Server::pchDir());
        for (int i=0; i<DatabaseTypeCount; ++i)
            RTags::removeDirectory(databaseDir(static_cast<Server::DatabaseType>(i)).constData());
        error() << "cleared database dir" << base;
    }
}

void Server::connectJob(Job *job)
{
    connect(job, SIGNAL(complete(int)), this, SLOT(onComplete(int)));
    connect(job, SIGNAL(output(int, QByteArray)), this, SLOT(onOutput(int, QByteArray)));
}
void Server::onSymbolNamesChanged()
{
    MatchJob *match = MatchJob::createCompletionMatchJob();
    connectJob(match);
    mCachedSymbolNames.clear();
    QThreadPool::globalInstance()->start(match);
}
