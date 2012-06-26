#include "Server.h"

#include "Event.h"
#include "Client.h"
#include <QObject>
#include "LocalClient.h"
#include "Connection.h"
#include "CursorInfoJob.h"
#include "Database.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "ListSymbolsJob.h"
#include "LocalServer.h"
#include "LogObject.h"
#include "MakefileMessage.h"
#include "MakefileParser.h"
#include "Message.h"
#include "Messages.h"
#include "CreateOutputMessage.h"
#include "Path.h"
#include "QueryMessage.h"
#include "Rdm.h"
#include "ReferencesJob.h"
#include "RegExp.h"
#include "RunTestJob.h"
#include "SHA256.h"
#include "StatusJob.h"
#include "TestJob.h"
#include "leveldb/cache.h"
#include "leveldb/db.h"
#include <Log.h>
#include <clang-c/Index.h>
#include <stdio.h>
#include <QCoreApplication>

Path Server::sBase;

class MakeEvent : public QEvent
{
public:
    MakeEvent()
        : QEvent(User)
    {}
    Path makefile;
    List<ByteArray> makefileArgs, extraFlags;
    Connection *connection;
};

Server *Server::sInstance = 0;
Server::Server(QObject *parent)
    : QObject(parent), mIndexer(0), mServer(0), mVerbose(false), mJobId(0), mThreadPool(0)
{
    Q_ASSERT(!sInstance);
    sInstance = this;
    qRegisterMetaType<Path>("Path");
    qRegisterMetaType<ByteArray>("ByteArray");
    qRegisterMetaType<List<ByteArray> >("List<ByteArray>");
    memset(mDBs, 0, sizeof(mDBs));
}

Server::~Server()
{
    delete mIndexer;
    clear();
    Q_ASSERT(sInstance = this);
    sInstance = 0;
}

void Server::clear()
{
    delete mServer;
    mServer = 0;
    for (int i=0; i<DatabaseTypeCount; ++i) {
        delete mDBs[i];
        mDBs[i] = 0;
    }
}

bool Server::init(const Options &options)
{
    mThreadPool = new ThreadPool(ThreadPool::idealThreadCount());

    mMakefilesWatcher.modified().connect(this, &Server::onMakefileModified);
    mMakefilesWatcher.removed().connect(this, &Server::onMakefileRemoved);

    mOptions = options;
    if (!(options.options & NoClangIncludePath)) {
        Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
        clangPath.prepend("-I");
        mOptions.defaultArguments.append(clangPath);
    }

    Messages::init();

    for (int i=0; i<10; ++i) {
        mServer = new LocalServer;
        if (mServer->listen(mOptions.name)) {
            break;
        }
        delete mServer;
        mServer = 0;
        if (!i) {
            Client client(mOptions.name, Client::DontWarnOnConnectionFailure);
            QueryMessage msg(QueryMessage::Shutdown);
            client.message(&msg);
        }
        sleep(1);
        Path::rm(mOptions.name);
    }
    if (!mServer) {
        error("Unable to listen on %s", mOptions.name.constData());
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
        ScopedDB general = Server::instance()->db(Server::General, ScopedDB::Write);
        bool ok;
        const int version = general->value<int>("version", &ok);
        if (!ok) {
            general->setValue<int>("version", Rdm::DatabaseVersion);
        } else if (version != Rdm::DatabaseVersion) {
            error("Wrong version, expected %d, got %d. Run with -C to regenerate database", Rdm::DatabaseVersion, version);
            return false;
        }
        mMakefiles = general->value<Map<Path, MakefileInformation> >("makefiles");
        error() << mMakefiles.keys();
        for (Map<Path, MakefileInformation>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
            mMakefilesWatcher.watch(it->first);
        }
    }

    {
        // fileids
        ScopedDB db = Server::instance()->db(Server::FileIds, ScopedDB::Read);
        RTags::Ptr<Iterator> it(db->createIterator());
        it->seekToFirst();
        Map<uint32_t, Path> idsToPaths;
        Map<Path, uint32_t> pathsToIds;
        uint32_t maxId = 0;
        while (it->isValid()) {
            const Slice key = it->key();
            const Path path(key.data(), key.size());
            const uint32_t fileId = it->value<uint32_t>();
            maxId = qMax(fileId, maxId);
            idsToPaths[fileId] = path;
            pathsToIds[path] = fileId;
            it->next();
        }
        Location::init(pathsToIds, idsToPaths, maxId);
    }
    mServer->clientConnected().connect(this, &Server::onNewConnection);

    error() << "running with " << mOptions.defaultArguments << " clang version " << Rdm::eatString(clang_getClangVersion());

    mIndexer = new Indexer;
    mIndexer->indexingDone().connect(this, &Server::onIndexingDone);

    remake();

    return true;
}

void Server::onNewConnection()
{
    forever {
        LocalClient *client = mServer->nextClient();
        if (!client)
            break;
        Connection *conn = new Connection(client);
        conn->newMessage().connect(this, &Server::onNewMessage);
        conn->destroyed().connect(this, &Server::onConnectionDestroyed);
    }
}

void Server::onConnectionDestroyed(Connection *o)
{
    {
        Map<int, Connection*>::iterator it = mPendingIndexes.begin();
        const Map<int, Connection*>::const_iterator end = mPendingIndexes.end();
        while (it != end) {
            if (it->second == o) {
                mPendingIndexes.erase(it++);
            } else {
                ++it;
            }
        }
    }
    {
        Map<int, Connection*>::iterator it = mPendingLookups.begin();
        const Map<int, Connection*>::const_iterator end = mPendingLookups.end();
        while (it != end) {
            if (it->second == o) {
                mPendingLookups.erase(it++);
            } else {
                ++it;
            }
        }
    }
}

void Server::onNewMessage(Message *message, Connection *connection)
{
    switch (message->messageId()) {
    case MakefileMessage::MessageId:
        handleMakefileMessage(static_cast<MakefileMessage*>(message), connection);
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(static_cast<QueryMessage*>(message), connection);
        break;
    case ErrorMessage::MessageId:
        handleErrorMessage(static_cast<ErrorMessage*>(message), connection);
        break;
    case CreateOutputMessage::MessageId:
        handleCreateOutputMessage(static_cast<CreateOutputMessage*>(message), connection);
        break;
    case ResponseMessage::MessageId:
    default:
        error("Unknown message: %d", message->messageId());
        break;
    }
}

void Server::handleMakefileMessage(MakefileMessage *message, Connection *conn)
{
    const Path makefile = message->makefile();
    const MakefileInformation mi(makefile.lastModified(), message->arguments(), message->extraFlags());
    mMakefiles[makefile] = mi;
    mMakefilesWatcher.watch(makefile);
    ScopedDB general = Server::instance()->db(Server::General, ScopedDB::Write);
    general->setValue("makefiles", mMakefiles);
    MakeEvent *ev = new MakeEvent;
    ev->makefile = message->makefile();
    ev->makefileArgs = message->arguments();
    ev->extraFlags = message->extraFlags();
    ev->connection = conn;
    // QCoreApplication::postEvent(this, ev);
    QMetaObject::invokeMethod(this, "make", Q_ARG(Path, message->makefile()));
                              // Q_ARG(List<ByteArray>(), message->arguments()),
                              // Q_ARG(List<ByteArray>(), message->extraFlags()),
                              // Q_ARG(Connection *, conn));
    conn->finish();

    // make(message->makefile(), message->arguments(), message->extraFlags(), conn);
}

void Server::make(const Path &path, List<ByteArray> makefileArgs, const List<ByteArray> &extraFlags, Connection *conn)
{
    MakefileParser *parser = new MakefileParser(extraFlags, conn);
    parser->fileReady().connect(this, &Server::onFileReady);
    parser->done().connect(this, &Server::onMakefileParserDone);
    if (mOptions.options & UseDashB)
        makefileArgs.append("-B");
    parser->run(path, makefileArgs);
}

void Server::onMakefileParserDone(MakefileParser *parser)
{
    assert(parser);
    Connection *connection = parser->connection();
    if (connection) {
        char buf[1024];
        if (parser->pchCount()) {
            snprintf(buf, sizeof(buf), "Parsed %s, %d sources, %d pch headers",
                     parser->makefile().constData(), parser->sourceCount(), parser->pchCount());
        } else {
            snprintf(buf, sizeof(buf), "Parsed %s, %d sources",
                     parser->makefile().constData(), parser->sourceCount());
        }
        ResponseMessage msg(buf);
        connection->send(&msg);
        connection->finish();
    }
    parser->deleteLater();
}

void Server::handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn)
{
    new LogObject(conn, message->level());
}

void Server::handleQueryMessage(QueryMessage *message, Connection *conn)
{
    int id = 0;
    switch (message->type()) {
    case QueryMessage::Invalid:
        Q_ASSERT(0);
        break;
    case QueryMessage::Reindex: {
        reindex(message->query().value(0));
        conn->finish();
        return; }
    case QueryMessage::Remake: {
        remake(message->query().value(0), conn);
        return; }
    case QueryMessage::ClearDatabase: {
        delete mThreadPool;
        mThreadPool = 0;
        clear();
        Server::setBaseDirectory(sBase, true);
        ResponseMessage msg("Cleared data dir");
        init(mOptions);
        conn->send(&msg);
        conn->finish();
        return; }
    case QueryMessage::FixIts:
        fixIts(*message, conn);
        return;
    case QueryMessage::Errors:
        errors(*message, conn);
        return;
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
        id = listSymbols(*message);
        break;
    case QueryMessage::FindSymbols:
        id = findSymbols(*message);
        break;
    case QueryMessage::Status:
        id = status(*message);
        break;
    case QueryMessage::Test:
        id = test(*message);
        break;
    case QueryMessage::RunTest:
        id = runTest(*message);
        break;
    }
    if (!id) {
        ResponseMessage msg;
        conn->send(&msg);
        conn->finish();
    } else {
        mPendingLookups[id] = conn;
    }
}

void Server::handleErrorMessage(ErrorMessage *message, Connection *)
{
    error("Error message: %s", message->message().constData());
}

void Server::onIndexingDone(int id)
{
    Map<int, Connection*>::iterator it = mPendingIndexes.find(id);
    if (it == mPendingIndexes.end())
        return;
    ErrorMessage msg("Hello, world");
    it->second->send(&msg);
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

    error("rc -f %s", loc.key().constData());

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

    error("rc -U %s", loc.key().constData());

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

    error("rc -r %s", loc.key().constData());

    ReferencesJob *job = new ReferencesJob(id, loc, query.flags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}

int Server::referencesForName(const QueryMessage& query)
{
    const int id = nextId();

    const ByteArray name = query.query().value(0);
    error("rc -R \"%s\"", name.constData());

    ReferencesJob *job = new ReferencesJob(id, name, query.flags());
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);

    return id;
}

int Server::findSymbols(const QueryMessage &query)
{
    const ByteArray partial = query.query().value(0);
    const int id = nextId();

    error("rc -F \"%s\"", partial.constData());

    FindSymbolsJob *job = new FindSymbolsJob(id, query);
    startJob(job);

    return id;
}

int Server::listSymbols(const QueryMessage &query)
{
    const ByteArray partial = query.query().value(0);
    const int id = nextId();

    error("rc -S \"%s\"", partial.constData());

    ListSymbolsJob *job = new ListSymbolsJob(id, query);
    startJob(job);

    return id;
}

int Server::status(const QueryMessage &query)
{
    const int id = nextId();

    error("rc -s \"%s\"", query.query().value(0).constData());

    StatusJob *job = new StatusJob(id, query.query().value(0));
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
    startJob(job);
    return id;
}

int Server::runTest(const QueryMessage &query)
{
    Path path = query.query().value(0);
    if (!path.isFile()) {
        return 0;
    }
    const int id = nextId();

    error("rc -T \"%s\"", path.constData());

    RunTestJob *job = new RunTestJob(path, id);
    startJob(job);
    return id;
}

int Server::test(const QueryMessage &query)
{
    Path path = query.query().value(0);
    if (!path.isFile()) {
        return 0;
    }
    const int id = nextId();

    error("rc -t \"%s\"", path.constData());

    TestJob *job = new TestJob(path, id);
    startJob(job);
    return id;
}

void Server::fixIts(const QueryMessage &query, Connection *conn)
{
    const ByteArray fixIts = mIndexer->fixIts(query.query().value(0));

    error("rc -x \"%s\"", fixIts.constData());

    ResponseMessage msg(fixIts);
    conn->send(&msg);
    conn->finish();
}

void Server::errors(const QueryMessage &query, Connection *conn)
{
    const ByteArray errors = mIndexer->errors(query.query().value(0));

    error("rc -Q \"%s\"", errors.constData());

    ResponseMessage msg(errors);
    conn->send(&msg);
    conn->finish();
}

void Server::rdmLog(const QueryMessage &query, Connection *conn)
{
    const char *q = query.query().front().constData();
    const int level = *reinterpret_cast<const int *>(q);
    new LogObject(conn, level);
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

bool Server::setBaseDirectory(const ByteArray& base, bool clear)
{
    sBase = base;
    if (!sBase.endsWith('/'))
        sBase.append('/');
    if (!Path::mkdir(sBase)) {
        error("Can't create directory [%s]", sBase.constData());
        return false;
    }
    if (!sBase.mksubdir("pch")) {
        error("Can't create directory [%s/pch]", sBase.constData());
        return false;
    }

    if (clear) {
        RTags::removeDirectory(Server::pchDir().constData());
        for (int i=0; i<DatabaseTypeCount; ++i)
            RTags::removeDirectory(databaseDir(static_cast<Server::DatabaseType>(i)).constData());
        error() << "cleared database dir" << base;
    }
    return true;
}

void Server::reindex(const ByteArray &pattern)
{
    mIndexer->reindex(pattern);
}

void Server::remake(const ByteArray &pattern, Connection *conn)
{
    error() << "remake " << pattern;
    RegExp rx(pattern);
    for (Map<Path, MakefileInformation>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
        if (rx.isEmpty() || rx.indexIn(it->first) != -1) {
            if (conn) {
                ResponseMessage msg("Remaking " + it->first);
                conn->send(&msg);
            }
            make(it->first, it->second.makefileArgs, it->second.extraFlags, conn);
        }
    }
    if (conn) {
        conn->finish();
    }
}

void Server::startJob(Job *job)
{
    mThreadPool->start(job, job->priority());
}

void Server::onFileReady(const GccArguments &args)
{
    if (args.inputFiles().isEmpty()) {
        warning("no input file?");
        return;
    } else if (args.outputFile().isEmpty()) {
        warning("no output file?");
        return;
    } else if (args.type() == GccArguments::NoType || args.lang() == GccArguments::NoLang) {
        return;
    }

    MakefileParser *parser = qobject_cast<MakefileParser*>(sender());

    if (args.type() == GccArguments::Pch) {
        ByteArray output = args.outputFile();
        Q_ASSERT(!output.isEmpty());
        const int ext = output.lastIndexOf(".gch/c");
        if (ext != -1) {
            output = output.left(ext + 4);
        } else if (!output.endsWith(".gch")) {
            error("couldn't find .gch in pch output");
            return;
        }
        const ByteArray input = args.inputFiles().front();
        parser->setPch(output, input);
    }

    List<ByteArray> arguments = args.clangArgs();
    if (args.lang() == GccArguments::CPlusPlus) {
        foreach(const ByteArray &pch, parser->mapPchToInput(args.explicitIncludes())) {
            arguments.append("-include-pch");
            arguments.append(pch);
        }
    }
    arguments.append(mOptions.defaultArguments);

    foreach(const ByteArray &input, args.inputFiles()) {
        if (arguments != Rdm::compileArgs(Location::insertFile(input))) {
            mIndexer->index(input, arguments, IndexerJob::Makefile);
        } else {
            warning() << input << "is not dirty. ignoring";
        }
    }
}
void Server::onMakefileModified(const Path &path)
{
    remake(path, 0);
}

void Server::onMakefileRemoved(const Path &path)
{
    mMakefiles.remove(path);
    ScopedDB general = Server::instance()->db(Server::General, ScopedDB::Write);
    general->setValue("makefiles", mMakefiles);
}

void Server::event(const Event *event)
{
    switch (event->type()) {
    case JobCompleteEvent::Type: {
        const JobCompleteEvent *e = static_cast<const JobCompleteEvent*>(event);
        Map<int, Connection*>::iterator it = mPendingLookups.find(e->job->id());
        if (it == mPendingLookups.end())
            return;
        it->second->finish();
        break; }
    case JobOutputEvent::Type: {
        const JobOutputEvent *e = static_cast<const JobOutputEvent*>(event);
        Map<int, Connection*>::iterator it = mPendingLookups.find(e->job->id());
        if (it == mPendingLookups.end())
            break;
        ResponseMessage msg(e->out);
        it->second->send(&msg);
        break; }
    default:
        assert(0);
        break;
    }
}

bool Server::event(QEvent *event)
{
    if (event->type() == QEvent::User) {
        MakeEvent *ev = static_cast<MakeEvent*>(event);
        make(ev->makefile, ev->makefileArgs, ev->extraFlags, ev->connection);
    }
    return QObject::event(event);
}
