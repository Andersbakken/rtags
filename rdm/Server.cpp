#include "Connection.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "Client.h"
#include "CursorInfoJob.h"
#include "MatchJob.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "TestJob.h"
#include "QueryMessage.h"
#include "Rdm.h"
#include "ReferencesJob.h"
#include "SHA256.h"
#include "IndexerJob.h"
#include "Server.h"
#include "StatusJob.h"
#include "Database.h"
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
Server::Server(QObject *parent)
    : QObject(parent), mIndexer(0), mServer(0), mVerbose(false), mJobId(0)
{
    Q_ASSERT(!sInstance);
    sInstance = this;
    qRegisterMetaType<QList<QByteArray> >("QList<QByteArray>");
    memset(mDBs, 0, sizeof(mDBs));
    QThreadPool::globalInstance()->setExpiryTimeout(-1);
}

Server::~Server()
{
    for (int i=0; i<DatabaseTypeCount; ++i) {
        delete mDBs[i];
    }

    Q_ASSERT(sInstance = this);
    sInstance = 0;
}

static inline QList<Path> systemIncludes(const Path &cpp)
{
    QList<Path> systemIncludes;
    QProcess proc;
    proc.start(cpp, QStringList() << QLatin1String("-v"));
    proc.closeWriteChannel();
    proc.waitForFinished();
    QList<QByteArray> lines = proc.readAllStandardError().split('\n');
    bool seenInclude = false;
    Path gxxIncludeDir;
    QByteArray target;
    foreach(const QByteArray& line, lines) {
        if (gxxIncludeDir.isEmpty()) {
            int idx = line.indexOf("--with-gxx-include-dir=");
            if (idx != -1) {
                const int space = line.indexOf(' ', idx);
                gxxIncludeDir = line.mid(idx + 23, space - idx - 23);
                if (!gxxIncludeDir.resolve())
                    gxxIncludeDir.clear();
            }
            idx = line.indexOf("--target=");
            if (idx != -1) {
                const int space = line.indexOf(' ', idx);
                target = line.mid(idx + 9, space - idx - 9);
            }
        } else if (!seenInclude && line.startsWith("#include ")) {
            seenInclude = true;
        } else if (seenInclude && line.startsWith(" /")) {
            Path path = Path::resolved(line.mid(1));
            if (path.isDir()) {
                systemIncludes.append(path);
            }
        }
    }
    if (gxxIncludeDir.isDir()) {
        systemIncludes.append(gxxIncludeDir);
        if (!target.isEmpty()) {
            gxxIncludeDir += target;
            if (!gxxIncludeDir.endsWith('/'))
                gxxIncludeDir.append('/');
            if (gxxIncludeDir.isDir())
                systemIncludes.append(gxxIncludeDir);
        }
    }
    return systemIncludes;
}

bool Server::init(const Options &options)
{
    mName = options.name;
    mOptions = options.options;
    if (!(options.options & NoClangIncludePath)) {
        Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
        clangPath.prepend("-I");
        mDefaultArgs.append(clangPath);
    }

    mDefaultArgs += options.defaultArguments;
    Messages::init();

    for (int i=0; i<10; ++i) {
        mServer = new QLocalServer(this);
        if (mServer->listen(mName)) {
            break;
        }
        delete mServer;
        mServer = 0;
        if (!i) {
            Client client(mName);
            QueryMessage msg(QueryMessage::Shutdown);
            client.query(&msg);
        }
        sleep(1);
        QFile::remove(mName);
    }
    if (!mServer) {
        error("Unable to listen to port %d", Connection::Port);
        return false;
    }

    for (int i=0; i<DatabaseTypeCount; ++i) {
        mDBs[i] = new Database(databaseDir(static_cast<DatabaseType>(i)).constData(),
                               options.cacheSizeMB, i == Server::Symbol);
        if (!mDBs[i]->isOpened()) {
            error() << "Failed to open db" << mDBs[i]->openError();
            return false;
        }
    }

    {
        // fileids
        ScopedDB db = Server::instance()->db(Server::FileIds, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        QHash<quint32, Path> idsToPaths;
        QHash<Path, quint32> pathsToIds;
        quint32 maxId = 0;
        while (it->isValid()) {
            const Slice key = it->key();
            const Path path(key.data(), key.size());
            const quint32 fileId = it->value<quint32>();
            maxId = qMax(fileId, maxId);
            idsToPaths[fileId] = path;
            pathsToIds[path] = fileId;
            it->next();
        }
        Location::init(pathsToIds, idsToPaths, maxId);
    }

    {
        ScopedDB general = Server::instance()->db(Server::General, ScopedDB::Write);
        bool ok;
        const int version = general->value<int>("version", &ok);
        if (!ok) {
            general->setValue<int>("version", Rdm::DatabaseVersion);
        } else if (version != Rdm::DatabaseVersion) {
            error("Wrong version, expected %d, got %d. Run with -C to regenerate database", version, Rdm::DatabaseVersion);
            return false;
        }
    }


    connect(mServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    error() << "running with" << mDefaultArgs << "clang version" << Rdm::eatString(clang_getClangVersion());

    mIndexer = new Indexer(sBase, this);
    connect(mIndexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));
    connect(mIndexer, SIGNAL(jobsComplete()), this, SLOT(onSymbolNamesChanged()));

    onSymbolNamesChanged();
    return true;
}

void Server::onNewConnection()
{
    while (mServer->hasPendingConnections()) {
        QLocalSocket *socket = mServer->nextPendingConnection();
        Connection *conn = new Connection(socket, this);
        connect(conn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
        connect(socket, SIGNAL(disconnected()), conn, SLOT(deleteLater()));
        connect(conn, SIGNAL(destroyed(QObject*)), this, SLOT(onConnectionDestroyed(QObject*)));
    }
}

void Server::onConnectionDestroyed(QObject *o)
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

void Server::onNewMessage(Message *message)
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
    case ResponseMessage::MessageId:
    default:
        error("Unknown message: %d", message->messageId());
        break;
    }

    message->deleteLater();
}

void Server::handleAddMessage(AddMessage *message)
{
    Connection *conn = qobject_cast<Connection*>(sender());
    if (!conn->property("connected").toBool()) {
        connect(mIndexer, SIGNAL(jobsComplete()), conn, SLOT(finish())); // ### this is kind of a hack
        conn->setProperty("connected", true);
    }

    /* We want to have the arguments in this order:
       1) flags from Makefile
       2) flags from clang
       3) flags from compiler
    */
    QList<QByteArray> args = message->arguments();

    if (!message->compiler().isEmpty() && false) {
        static bool first = true;
        static QHash<Path, QList<QByteArray> > compilerFlags;
        if (first) {
            QSettings settings(QSettings::IniFormat, QSettings::UserScope, "RTags");
            settings.beginGroup("Compilers");
            foreach(QString key, settings.childKeys()) {
                QByteArray val = settings.value(key).toByteArray();
                key.replace('!', '/');
                compilerFlags[key.toLocal8Bit()] = val.split(' ');
            }
            warning() << "Read" << compilerFlags << "from settings" << settings.fileName();
            first = false;
        }
        QList<QByteArray> &flags = compilerFlags[message->compiler()];
        if (flags.isEmpty()) {
            const Path cpp = message->compiler().parentDir() + "/cpp";
            if (cpp.isFile()) {
                foreach(const Path &systemPath, systemIncludes(cpp)) {
                    flags.append("-I" + systemPath);
                }
            }
            if (flags.isEmpty()) { // make sure we don't look this up every time
                flags.append(QByteArray());
            }
        }
        if (flags.size() != 1 || !flags.at(0).isEmpty())
            args += flags;
        // warning() << "got" << flags << "for" << message->compiler() << "now we have" << args;
    }
    args += mDefaultArgs;

    if (args != Rdm::compileArgs(Location::insertFile(message->inputFile()))) {
        // if (!Rdm::compileArgs(Location::insertFile(message->inputFile())).isEmpty()) {
        //     qDebug() << message->inputFile() << RTags::join(args, " ")
        //              << "vs"
        //              << RTags::join(Rdm::compileArgs(Location::insertFile(message->inputFile())), " ");
        // }
        const int id = mIndexer->index(message->inputFile(), args, IndexerJob::Makefile);
        if (id != -1)
            mPendingIndexes[id] = conn;
    }
}

void Server::handleQueryMessage(QueryMessage *message)
{
    Connection *conn = qobject_cast<Connection*>(sender());
    int id = 0;
    switch (message->type()) {
    case QueryMessage::Invalid:
        Q_ASSERT(0);
        break;
    case QueryMessage::CursorInfo:
        id = cursorInfo(*message);
        break;
    case QueryMessage::Shutdown:
        QCoreApplication::instance()->quit();
        conn->finish();
        return;
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
        if (message->query().value(0).isEmpty() && !message->flags() && !mCachedSymbolNames.isEmpty()) {
            ResponseMessage response(mCachedSymbolNames);
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
    case QueryMessage::RdmLog:
        rdmLog(*message, conn);
        return;
    }
    if (!id) {
        ResponseMessage msg;
        conn->send(&msg);
        conn->finish();
    } else {
        mPendingLookups[id] = conn;
    }
}

void Server::handleErrorMessage(ErrorMessage *message)
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
    ResponseMessage msg(response);
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
    const Location loc = Location::decodeClientLocation(query.query().value(0));
    if (loc.isNull()) {
        return 0;
    }

    const int id = nextId();

    error() << "followLocation" << loc;

    FollowLocationJob *job = new FollowLocationJob(id, loc, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}

int Server::cursorInfo(const QueryMessage &query)
{
    const Location loc = Location::decodeClientLocation(query.query().value(0));
    if (loc.isNull()) {
        return 0;
    }

    const int id = nextId();

    error() << "cursorInfo" << loc;

    CursorInfoJob *job = new CursorInfoJob(id, loc, query.keyFlags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}


int Server::referencesForLocation(const QueryMessage &query)
{
    const Location loc = Location::decodeClientLocation(query.query().value(0));
    if (loc.isNull()) {
        return 0;
    }

    const int id = nextId();

    error() << "references for location" << loc;

    ReferencesJob *job = new ReferencesJob(id, loc, query.flags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}

int Server::referencesForName(const QueryMessage& query)
{
    const int id = nextId();

    const QByteArray name = query.query().value(0);
    error() << "references for name" << name;

    ReferencesJob *job = new ReferencesJob(id, name, query.flags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}

int Server::match(const QueryMessage &query)
{
    const QByteArray partial = query.query().value(0);
    const int id = nextId();

    error() << "match" << partial;

    MatchJob *job = new MatchJob(id, query);
    startJob(job);

    return id;
}

int Server::dump(const QueryMessage &query)
{
    const QByteArray partial = query.query().value(0);
    const int id = nextId();

    error() << "dump" << partial;

    DumpJob *job = new DumpJob(partial, id);
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}

int Server::status(const QueryMessage &query)
{
    const int id = nextId();

    error() << "status" << query.query().value(0);

    StatusJob *job = new StatusJob(id, query.query().value(0));
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);
    return id;
}

int Server::test(const QueryMessage &query)
{
    const int id = nextId();

    error() << "test";

    TestJob *job = new TestJob(query.query().value(0), id);
    startJob(job);
    return id;
}

void Server::rdmLog(const QueryMessage &query, Connection *conn)
{
    const char *q = query.query().first().constData();
    const int level = *reinterpret_cast<const int *>(q);
    new Rdm::LogObject(conn, level);
}

static const char *const dbNames[] = {
    "general.db",
    "dependencies.db",
    "symbols.db",
    "symbolnames.db",
    "fileinfos.db",
    "pchusrhashes.db",
    "fileids.db",
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
    return sBase + "pch/";
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

void Server::startJob(Job *job)
{
    connect(job, SIGNAL(complete(int)), this, SLOT(onComplete(int)));
    connect(job, SIGNAL(output(int, QByteArray)), this, SLOT(onOutput(int, QByteArray)));
    QThreadPool::globalInstance()->start(job, job->priority());
}
void Server::onSymbolNamesChanged()
{
    MatchJob *match = MatchJob::createCompletionMatchJob();
    mCachedSymbolNames.clear();
    startJob(match);
}

ScopedDB::ScopedDB(Database *db, LockType lockType)
    : mData(new Data(db, lockType))
{
}

ScopedDB::Data::Data(Database *database, LockType lockType)
    : db(database), lock(lockType)
{
    if (db && lockType == Write) {
        db->lockForWrite();
    }
}

ScopedDB::Data::~Data()
{
    if (db && lock == Write)
        db->unlock();
}
