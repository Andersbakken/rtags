#include "Server.h"

#include "Client.h"
#include "Connection.h"
#include "CreateOutputMessage.h"
#include "CursorInfoJob.h"
#include "Event.h"
#include "EventLoop.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "ListSymbolsJob.h"
#include "LocalClient.h"
#include "LocalServer.h"
#include "LogObject.h"
#include "MakefileMessage.h"
#include "MakefileParser.h"
#include "Message.h"
#include "Messages.h"
#include "ParseJob.h"
#include "Path.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include "RegExp.h"
#include "RunTestJob.h"
#include "SHA256.h"
#include "StatusJob.h"
#include "TestJob.h"
#include "leveldb/cache.h"
#include "leveldb/db.h"
#include "GRTagsMessage.h"
#include <Log.h>
#include <clang-c/Index.h>
#include <dirent.h>
#include <fnmatch.h>
#include <stdio.h>

class MakefileParserDoneEvent : public Event
{
public:
    enum { Type = 4 };
    MakefileParserDoneEvent(MakefileParser *p)
        : Event(Type), parser(p)
    {}
    MakefileParser *parser;
};

Server *Server::sInstance = 0;
Server::Server()
    : mServer(0), mVerbose(false), mJobId(0), mThreadPool(0)
{
    assert(!sInstance);
    sInstance = this;
}

Server::~Server()
{
    clear();
    assert(sInstance == this);
    sInstance = 0;
    Messages::cleanup();
}

void Server::clear()
{
    if (mThreadPool) {
        mThreadPool->clearBackLog();
        delete mThreadPool;
        mThreadPool = 0;
    }
    Map<Path, shared_ptr<Project> >::iterator it = mProjects.begin();
    while (it != mProjects.end()) {
        if (it->second->indexer) {
            it->second->indexer->abort();
        }
        ++it;
    }

    Path::rm(mOptions.socketFile);
    delete mServer;
    mServer = 0;
    mProjects.clear();
    setCurrentProject(0);
}

bool Server::init(const Options &options)
{
    mThreadPool = new ThreadPool(options.threadCount);

    mMakefilesWatcher.modified().connect(this, &Server::onMakefileModified);
    // mMakefilesWatcher.removed().connect(this, &Server::onMakefileRemoved);

    mOptions = options;
    if (!(options.options & NoClangIncludePath)) {
        Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
        clangPath.prepend("-I");
        mOptions.defaultArguments.append(clangPath);
    }
    mClangPath = Path::resolved(CLANG_BIN "/clang");
    error() << "using args" << mOptions.defaultArguments;

    Messages::init();
    if (mOptions.options & ClearProjects) {
        clearProjects();
    }

    for (int i=0; i<10; ++i) {
        mServer = new LocalServer;
        if (mServer->listen(mOptions.socketFile)) {
            break;
        }
        delete mServer;
        mServer = 0;
        if (!i) {
            Client client(mOptions.socketFile, Client::DontWarnOnConnectionFailure);
            QueryMessage msg(QueryMessage::Shutdown);
            client.message(&msg);
        }
        sleep(1);
        Path::rm(mOptions.socketFile);
    }
    if (!mServer) {
        error("Unable to listen on %s", mOptions.socketFile.constData());
        return false;
    }

    // {
    //     bool ok;
    //     const int version = mDB->value<int>("version", &ok);
    //     if (!ok) {
    //         mDB->setValue<int>("version", Database::Version);
    //     } else if (version != Database::Version) {
    //         error("Wrong version, expected %d, got %d. Run with -C to regenerate database", Database::Version, version);
    //         return false;
    //     }
    //     mMakefiles = mDB->value<Map<Path, MakefileInformation> >("makefiles");
    //     for (Map<Path, MakefileInformation>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
    //         mMakefilesWatcher.watch(it->first);
    //     }
    // }

    mServer->clientConnected().connect(this, &Server::onNewConnection);

    if (!(mOptions.options & NoValidate))
        remake();

    return true;
}

void Server::onNewConnection()
{
    while (true) {
        LocalClient *client = mServer->nextClient();
        if (!client)
            break;
        Connection *conn = new Connection(client);
        conn->newMessage().connect(this, &Server::onNewMessage);
        conn->destroyed().connect(this, &Server::onConnectionDestroyed);
        // client->disconnected().connect(conn, &Connection::onLoop);
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
    case GRTagsMessage::MessageId:
        handleGRTagMessage(static_cast<GRTagsMessage*>(message), connection);
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
    // mDB->setValue("makefiles", mMakefiles);
    make(message->makefile(), message->arguments(), message->extraFlags(), conn);
}

void Server::handleGRTagMessage(GRTagsMessage *message, Connection *conn)
{
    const Path dir = message->path();
    shared_ptr<Project> proj = initProject(dir, EnableGRTags);
    if (proj)
        mCurrentProject = proj;
    conn->finish();
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
        snprintf(buf, sizeof(buf), "Parsed %s, %d sources",
                 parser->makefile().constData(), parser->sourceCount());
        ResponseMessage msg(buf);
        connection->send(&msg);
        connection->finish();
    }
    EventLoop::instance()->postEvent(this, new MakefileParserDoneEvent(parser));
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
        assert(0);
        break;
    case QueryMessage::FindFile:
        id = findFile(*message);
        break;
    case QueryMessage::Parse:
        id = parse(*message);
        break;
    case QueryMessage::DeleteProject: {
        RegExp rx(message->query());
        Map<Path, shared_ptr<Project> >::iterator it = mProjects.begin();
        while (it != mProjects.end()) {
            if (rx.indexIn(it->first) != -1) {
                if (it->second->indexer)
                    it->second->indexer->abort();
                ResponseMessage msg("Erased project: " + it->first);
                conn->send(&msg);
                if (it->second.get() == mCurrentProject.get()) {
                    mCurrentProject.reset();
                }
                mProjects.erase(it++);
            } else {
                ++it;
            }
        }
        if (!mCurrentProject && !mProjects.isEmpty()) {
            mCurrentProject = mProjects.begin()->second;
        }
        break; }
    case QueryMessage::Project:
        if (message->query().isEmpty()) {
            for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                ByteArray b = it->first;
                if (it->second == mCurrentProject)
                    b.append(" <=");
                ResponseMessage msg(b);
                conn->send(&msg);
            }
        } else {
            shared_ptr<Project> selected;
            bool error = false;
            RegExp rx(message->query());
            for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                if (rx.indexIn(it->first) != -1) {
                    if (error) {
                        ResponseMessage msg(it->first);
                        conn->send(&msg);
                    } else if (selected) {
                        error = true;
                        ResponseMessage msg("Multiple matches for " + message->query());
                        conn->send(&msg);
                        msg.setData(it->first);
                        conn->send(&msg);
                    } else {
                        selected = it->second;
                    }
                }
            }
            if (!error && selected) {
                setCurrentProject(selected);
                ResponseMessage msg("Selected project: " + selected->srcRoot);
                conn->send(&msg);
            }
        }
        conn->finish();
        return;
    case QueryMessage::Reindex: {
        reindex(message->query());
        conn->finish();
        return; }
    case QueryMessage::Remake: {
        remake(message->query(), conn);
        return; }
    case QueryMessage::ClearProjects: {
        delete mThreadPool;
        mThreadPool = 0;
        clear();
        clearProjects();
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
        EventLoop::instance()->exit();
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

int Server::nextId()
{
    ++mJobId;
    if (!mJobId)
        ++mJobId;
    return mJobId;
}

int Server::followLocation(const QueryMessage &query)
{
    const Location loc = Location::decodeClientLocation(query.query());
    if (loc.isNull()) {
        return 0;
    }
    updateProjectForLocation(loc);

    error("rc -f %s", loc.key().constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    FollowLocationJob *job = new FollowLocationJob(loc, query, project);
    job->setId(nextId());
    startJob(job);

    return job->id();
}

int Server::parse(const QueryMessage &query)
{
    error("rc -y %s", query.query().constData());
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    ParseJob *job = new ParseJob(query, project);
    job->setId(nextId());
    startJob(job);
    return job->id();
}

int Server::findFile(const QueryMessage &query)
{
    error("rc -P %s", query.query().constData());
    shared_ptr<Project> project = currentProject();
    if (!project || !project->grtags) {
        error("No project");
        return 0;
    }

    FindFileJob *job = new FindFileJob(query, project);
    job->setId(nextId());
    startJob(job);
    return job->id();
}

int Server::cursorInfo(const QueryMessage &query)
{
    const Location loc = Location::decodeClientLocation(query.query());
    if (loc.isNull()) {
        return 0;
    }
    updateProjectForLocation(loc);

    error("rc -U %s", loc.key().constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    CursorInfoJob *job = new CursorInfoJob(loc, query, project);
    job->setId(nextId());
    startJob(job);

    return job->id();
}


int Server::referencesForLocation(const QueryMessage &query)
{
    const Location loc = Location::decodeClientLocation(query.query());
    if (loc.isNull()) {
        return 0;
    }
    updateProjectForLocation(loc);

    error("rc -r %s", loc.key().constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    ReferencesJob *job = new ReferencesJob(loc, query, project);
    job->setId(nextId());

    startJob(job);

    return job->id();
}

int Server::referencesForName(const QueryMessage& query)
{
    const ByteArray name = query.query();
    error("rc -R \"%s\"", name.constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    ReferencesJob *job = new ReferencesJob(name, query, project);
    job->setId(nextId());
    startJob(job);

    return job->id();
}

int Server::findSymbols(const QueryMessage &query)
{
    const ByteArray partial = query.query();

    error("rc -F \"%s\"", partial.constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    FindSymbolsJob *job = new FindSymbolsJob(query, project);
    job->setId(nextId());
    startJob(job);

    return job->id();
}

int Server::listSymbols(const QueryMessage &query)
{
    const ByteArray partial = query.query();

    error("rc -S \"%s\"", partial.constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    ListSymbolsJob *job = new ListSymbolsJob(query, project);
    job->setId(nextId());
    startJob(job);

    return job->id();
}

int Server::status(const QueryMessage &query)
{
    error("rc -s \"%s\"", query.query().constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    StatusJob *job = new StatusJob(query, project);
    job->setId(nextId());
    startJob(job);
    return job->id();
}

int Server::runTest(const QueryMessage &query)
{
    Path path = query.query();
    if (!path.isFile()) {
        return 0;
    }
    error("rc -T \"%s\"", path.constData());

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        return 0;
    }

    RunTestJob *job = new RunTestJob(path, query, project);
    job->setId(nextId());
    startJob(job);
    return job->id();
}

int Server::test(const QueryMessage &query)
{
    Path path = query.query();
    if (!path.isFile()) {
        return 0;
    }
    error("rc -t \"%s\"", path.constData());

    TestJob *job = new TestJob(path);
    job->setId(nextId());
    startJob(job);
    return job->id();
}

void Server::fixIts(const QueryMessage &query, Connection *conn)
{
    error("rc -x \"%s\"", query.query().constData());

    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        error("No project");
        conn->finish();
        return;
    }

    const ByteArray fixIts = project->indexer->fixIts(query.query());

    ResponseMessage msg(fixIts);
    conn->send(&msg);
    conn->finish();
}

void Server::errors(const QueryMessage &query, Connection *conn)
{
    error("rc -Q \"%s\"", query.query().constData());
    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        error("No project");
        conn->finish();
        return;
    }

    const ByteArray errors = project->indexer->errors(query.query());

    ResponseMessage msg(errors);
    conn->send(&msg);
    conn->finish();
}

void Server::clearProjects()
{
    Path::rm(mOptions.projectsFile); // ### more stuff?
}

void Server::reindex(const ByteArray &pattern)
{
    shared_ptr<Project> project = currentProject();
    if (!project || project->indexer) {
        error("No project");
        return;
    }

    project->indexer->reindex(pattern);
}

void Server::remake(const ByteArray &pattern, Connection *conn)
{
    // error() << "remake " << pattern;
    RegExp rx(pattern);
    for (Map<Path, MakefileInformation>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
        if (rx.isEmpty() || rx.indexIn(it->first) != -1) {
            make(it->first, it->second.makefileArgs, it->second.extraFlags, conn);
        }
    }
}

void Server::startJob(Job *job)
{
    mThreadPool->start(job, Job::Priority);
}

/* Same behavior as rtags-default-current-project() */

enum FindAncestorFlag {
    Shallow = 0x1,
    Wildcard = 0x2
};
static inline Path findAncestor(Path path, const char *fn, unsigned flags)
{
    Path ret;
    int slash = path.size();
    const int len = strlen(fn) + 1;
    struct stat st;
    char buf[PATH_MAX + sizeof(dirent) + 1];
    dirent *direntBuf = 0, *entry = 0;
    if (flags & Wildcard)
        direntBuf = reinterpret_cast<struct dirent *>(malloc(sizeof(buf)));

    memcpy(buf, path.constData(), path.size() + 1);
    while ((slash = path.lastIndexOf('/', slash - 1)) > 0) { // We don't want to search in /
        if (!(flags & Wildcard)) {
            memcpy(buf + slash + 1, fn, len);
            if (!stat(buf, &st)) {
                buf[slash + 1] = '\0';
                ret = buf;
                if (flags & Shallow) {
                    break;
                }
            }
        } else {
            buf[slash + 1] = '\0';
            DIR *dir = opendir(buf);
            bool found = false;
            if (dir) {
                while (!readdir_r(dir, direntBuf, &entry) && entry) {
                    const int l = strlen(entry->d_name) + 1;
                    switch (l - 1) {
                    case 1:
                        if (entry->d_name[0] == '.')
                            continue;
                        break;
                    case 2:
                        if (entry->d_name[0] == '.' && entry->d_name[1] == '.')
                            continue;
                        break;
                    }
                    assert(buf[slash] == '/');
                    assert(l + slash + 1 < static_cast<int>(sizeof(buf)));
                    memcpy(buf + slash + 1, entry->d_name, l);
                    if (!fnmatch(fn, buf, 0)) {
                        ret = buf;
                        ret.truncate(slash + 1);
                        found = true;
                        break;
                    }
                }
            }
            closedir(dir);
            if (found && flags & Shallow)
                break;
        }
    }
    if (flags & Wildcard)
        free(direntBuf);

    return ret;
}

static Path findProjectRoot(const Path &path)
{
    struct Entry {
        const char *name;
        const unsigned flags;
    } entries[] = {
        { "GTAGS", 0 },
        { "configure", 0 },
        { ".git", 0 },
        { "CMakeLists.txt", 0 },
        { "*.pro", Wildcard },
        { "scons.1", 0 },
        { "*.scons", Wildcard },
        { "SConstruct", 0 },
        { "autogen.*", Wildcard },
        { "Makefile*", Wildcard },
        { "GNUMakefile*", Wildcard },
        { "INSTALL*", Wildcard },
        { "README*", Wildcard },
        { 0, 0 }
    };
    const Path home = Path::home();
    for (int i=0; entries[i].name; ++i) {
        const Path p = findAncestor(path, entries[i].name, entries[i].flags);
        if (!p.isEmpty() && p != home) {
            return p;
        }
    }

    {
        const Path configStatus = findAncestor(path, "config.status", 0);
        if (!configStatus.isEmpty()) {
            FILE *f = fopen((configStatus + "config.status").constData(), "r");
            char line[1024];
            enum { MaxLines = 10 };
            for (int i=0; i<MaxLines; ++i) {
                int r = RTags::readLine(f, line, sizeof(line));
                if (r == -1)
                    break;
                char *configure = strstr(line, "configure");
                if (configure) {
                    Path ret = Path::resolved(ByteArray(line, configure - line));
                    if (!ret.endsWith('/'))
                        ret.append('/');
                    if (ret != home)
                        return ret;
                }
            }
        }
    }
    return Path();
}

void Server::onFileReady(const GccArguments &args, MakefileParser *parser)
{
    List<Path> inputFiles = args.inputFiles();
    const int c = inputFiles.size();
    if (!c) {
        warning("no input file?");
        return;
    } else if (args.outputFile().isEmpty()) {
        warning("no output file?");
        return;
    } else if (args.type() == GccArguments::NoType || args.lang() == GccArguments::NoLang) {
        return;
    }
    const Path unresolved = *args.unresolvedInputFiles().begin();

    Path projectRoot = findProjectRoot(unresolved);
    if (projectRoot.isEmpty()) {
        projectRoot = findProjectRoot(*inputFiles.begin());
    }

    if (projectRoot.isEmpty()) {
        error("Can't find project root for %s", inputFiles.begin()->constData());
        return;
    }
    shared_ptr<Project> proj = initProject(projectRoot, EnableIndexer);
    if (!proj) {
        error("Can't find project for %s", projectRoot.constData());
        return;
    }
    mCurrentProject = proj;
    assert(proj->indexer);

    List<ByteArray> arguments = args.clangArgs();
    arguments.append(mOptions.defaultArguments);

    for (int i=0; i<c; ++i) {
        const Path &input = inputFiles.at(i);
        if (proj->indexer->compileArguments(Location::insertFile(input)) != arguments) {
            proj->indexer->index(input, arguments, IndexerJob::Makefile);
        } else {
            debug() << input << " is not dirty. ignoring";
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
    // mDB->setValue("makefiles", mMakefiles);
}

void Server::event(const Event *event)
{
    switch (event->type()) {
    case JobCompleteEvent::Type: {
        const JobCompleteEvent *e = static_cast<const JobCompleteEvent*>(event);
        Map<int, Connection*>::iterator it = mPendingLookups.find(e->id);
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
        if (it->second->isConnected() && !it->second->send(&msg)) {
            e->job->abort();
        }
        break; }
    case MakefileParserDoneEvent::Type: {
        delete static_cast<const MakefileParserDoneEvent*>(event)->parser;
        break; }
    default:
        EventReceiver::event(event);
        break;
    }
}

shared_ptr<Project> Server::setCurrentProject(const Path &path)
{
    Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.find(path);
    if (it != mProjects.end()) {
        setCurrentProject(it->second);
        return it->second;
    }
    return shared_ptr<Project>();
}

shared_ptr<Project> Server::initProject(const Path &path, unsigned flags)
{
    Path tmp = path;
    if (!tmp.endsWith('/'))
        tmp.append('/');
    shared_ptr<Project> &project = mProjects[tmp];
    if (!project) {
        project.reset(new Project);
        project->srcRoot = tmp;
        if (!RTags::encodePath(tmp)) {
            error("Invalid folder name %s", path.constData());
            mProjects.remove(path);
            return shared_ptr<Project>();
        }
    }

    if (flags & EnableIndexer && !project->indexer) {
        project->indexer = new Indexer(project, !(mOptions.options & NoValidate));
    }

    if (!project->grtags) {
        project->grtags = new GRTags;
        project->grtags->init(project, flags & EnableGRTags ? GRTags::Parse : GRTags::None);
    } else if (flags & EnableGRTags) {
        project->grtags->enableParsing();
    }

    return project;
}

bool Server::updateProjectForLocation(const Location &location)
{
    const Path path = location.path();
    Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.lower_bound(path);
    if (it != mProjects.begin()) {
        --it;
        if (!strncmp(it->first.constData(), path.constData(), it->first.size())) {
            setCurrentProject(it->second);
            return true;
        }
    }

    return false;
}

shared_ptr<Project> Server::setCurrentProject(const shared_ptr<Project> &proj)
{
    // if (proj) {
    //     ByteArray srcRoot = proj->srcRoot;
    //     if (!srcRoot.isEmpty()) {
    //         ScopedDB general = Server::instance()->db(Server::General, ReadWriteLock::Write);
    //         general->setValue<ByteArray>("currentProject", srcRoot);
    //     }
    // }
    shared_ptr<Project> old = mCurrentProject;
    mCurrentProject = proj;
    return old;
}

