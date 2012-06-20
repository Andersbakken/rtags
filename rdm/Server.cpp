#include "Connection.h"
#include "DumpJob.h"
#include "FollowLocationJob.h"
#include "MakefileParser.h"
#include "Indexer.h"
#include "Client.h"
#include "CursorInfoJob.h"
#include "ListSymbolsJob.h"
#include "FindSymbolsJob.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "TestJob.h"
#include "RunTestJob.h"
#include "QueryMessage.h"
#include "OutputMessage.h"
#include "MakefileMessage.h"
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

Server *Server::sInstance = 0;
Server::Server(QObject *parent)
    : QObject(parent), mIndexer(0), mServer(0), mVerbose(false), mJobId(0), mThreadPool(0)
{
    Q_ASSERT(!sInstance);
    sInstance = this;
    qRegisterMetaType<ByteArray>("ByteArray");
    qRegisterMetaType<List<ByteArray> >("List<ByteArray>");
    memset(mDBs, 0, sizeof(mDBs));
}

Server::~Server()
{
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

static inline List<Path> systemIncludes(const Path &cpp)
{
    List<Path> systemIncludes;
    QProcess proc;
    proc.start(cpp, QStringList() << QLatin1String("-v"));
    proc.closeWriteChannel();
    proc.waitForFinished();
    QByteArray ba = proc.readAllStandardError();
    List<ByteArray> lines = ByteArray(ba.constData(), ba.size()).split('\n');
    bool seenInclude = false;
    Path gxxIncludeDir;
    ByteArray target;
    foreach(const ByteArray& line, lines) {
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
    mThreadPool = new ThreadPool(ThreadPool::idealThreadCount());

    mOptions = options;
    if (!(options.options & NoClangIncludePath)) {
        Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
        clangPath.prepend("-I");
        mOptions.defaultArguments.append(clangPath);
    }

    Messages::init();

    for (int i=0; i<10; ++i) {
        mServer = new QLocalServer(this);
        if (mServer->listen(mOptions.name)) {
            break;
        }
        delete mServer;
        mServer = 0;
        if (!i) {
            Client client(mOptions.name);
            QueryMessage msg(QueryMessage::Shutdown);
            client.message(&msg);
        }
        sleep(1);
        QFile::remove(mOptions.name);
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
        ScopedDB general = Server::instance()->db(Server::General, ScopedDB::Write);
        bool ok;
        const int version = general->value<int>("version", &ok);
        if (!ok) {
            general->setValue<int>("version", Rdm::DatabaseVersion);
        } else if (version != Rdm::DatabaseVersion) {
            error("Wrong version, expected %d, got %d. Run with -C to regenerate database", Rdm::DatabaseVersion, version);
            return false;
        }
        mMakefiles = general->value<Map<Path, std::pair<List<ByteArray>, List<ByteArray> > > >("makefiles");
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


    connect(mServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));
    error() << "running with" << mOptions.defaultArguments << "clang version" << Rdm::eatString(clang_getClangVersion());

    mIndexer = new Indexer(sBase, this);
    connect(mIndexer, SIGNAL(indexingDone(int)), this, SLOT(onIndexingDone(int)));

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

void Server::onNewMessage(Message *message)
{
    switch (message->messageId()) {
    case MakefileMessage::MessageId:
        handleMakefileMessage(static_cast<MakefileMessage*>(message));
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(static_cast<QueryMessage*>(message));
        break;
    case ErrorMessage::MessageId:
        handleErrorMessage(static_cast<ErrorMessage*>(message));
        break;
    case OutputMessage::MessageId:
        handleOutputMessage(static_cast<OutputMessage*>(message));
        break;
    case ResponseMessage::MessageId:
    default:
        error("Unknown message: %d", message->messageId());
        break;
    }

    message->deleteLater();
}

void Server::handleMakefileMessage(MakefileMessage *message)
{
    mMakefiles[message->makefile()] = std::pair<List<ByteArray>, List<ByteArray> >(message->arguments(), message->extraFlags());
    ScopedDB general = Server::instance()->db(Server::General, ScopedDB::Write);
    general->setValue("makefiles", mMakefiles);
    make(message->makefile(), message->arguments(), message->extraFlags());
}

void Server::make(const Path &path, List<ByteArray> makefileArgs, const List<ByteArray> &extraFlags)
{
    MakefileParser *parser = new MakefileParser(extraFlags, this);
    connect(parser, SIGNAL(fileReady(GccArguments)), this, SLOT(onFileReady(GccArguments)));
    connect(parser, SIGNAL(done()), parser, SLOT(deleteLater()));
    if (mOptions.options & UseDashB)
        makefileArgs.append("-B");
    parser->run(path, makefileArgs);
    Connection *conn = qobject_cast<Connection*>(sender());
    conn->finish();
}

void Server::handleOutputMessage(OutputMessage *message)
{
    Connection *conn = qobject_cast<Connection*>(sender());
    const List<ByteArray> names = message->name().split(',');
    foreach(const ByteArray& name, names) {
        if (name == "log") {
            new Rdm::LogObject(conn, message->level());
        } else {
            const int level = Rdm::EventObject::typeForName(name);
            if (level == -1) {
                ResponseMessage msg("Unknown output name: " + name);
                conn->send(&msg);
                conn->finish();
                return;
            }
            new Rdm::EventObject(conn, level);
        }
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
    case QueryMessage::Dump:
        id = dump(*message);
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

void Server::handleErrorMessage(ErrorMessage *message)
{
    qWarning("Error message: %s", message->message().constData());
}

void Server::onIndexingDone(int id)
{
    Map<int, Connection*>::iterator it = mPendingIndexes.find(id);
    if (it == mPendingIndexes.end())
        return;
    ErrorMessage msg("Hello, world");
    it->second->send(&msg);
}

void Server::onComplete(int id)
{
    Map<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    it->second->finish();
}

void Server::onOutput(int id, const ByteArray &response)
{
    Map<int, Connection*>::iterator it = mPendingLookups.find(id);
    if (it == mPendingLookups.end())
        return;
    ResponseMessage msg(response);
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


int Server::dump(const QueryMessage &query)
{
    const ByteArray partial = query.query().value(0);
    const int id = nextId();

    error("rc -d \"%s\"", partial.constData());

    DumpJob *job = new DumpJob(partial, id);
    job->setPathFilters(query.pathFilters(), query.flags() & QueryMessage::FilterSystemIncludes);
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

void Server::setBaseDirectory(const ByteArray& base, bool clear)
{
    sBase = base;
    if (!sBase.endsWith('/'))
        sBase.append('/');
    Q_ASSERT(sBase.endsWith('/'));
    QDir dir;
    dir.mkpath(sBase);
    if (clear) {
        RTags::removeDirectory(Server::pchDir().constData());
        for (int i=0; i<DatabaseTypeCount; ++i)
            RTags::removeDirectory(databaseDir(static_cast<Server::DatabaseType>(i)).constData());
        error() << "cleared database dir" << base;
    }
}

void Server::reindex(const ByteArray &pattern)
{
    mIndexer->reindex(pattern);
}

void Server::remake(const ByteArray &pattern, Connection *conn)
{
    error() << "remake" << pattern;
    QRegExp rx(pattern);
    for (Map<Path, std::pair<List<ByteArray>, List<ByteArray> > >::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
        if (rx.isEmpty() || rx.indexIn(it->first) != -1) {
            qDebug() << "calling remake on" << it->first;
            ResponseMessage msg("Remaking " + it->first);
            conn->send(&msg);
            make(it->first, it->second.first, it->second.second);
        }
    }
    conn->finish();
}


void Server::startJob(Job *job)
{
    connect(job, SIGNAL(complete(int)), this, SLOT(onComplete(int)));
    connect(job, SIGNAL(output(int, ByteArray)), this, SLOT(onOutput(int, ByteArray)));
    mThreadPool->start(job, job->priority());
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
