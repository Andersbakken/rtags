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
#include "GRTags.h"
#include "GRTagsMessage.h"
#include "Indexer.h"
#include "IndexerJob.h"
#include "IniFile.h"
#include "ListSymbolsJob.h"
#include "LocalClient.h"
#include "LocalServer.h"
#include "LogObject.h"
#include "MakefileInformation.h"
#include "MakefileMessage.h"
#include "MakefileParser.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include "RegExp.h"
#include "SHA256.h"
#include "StatusJob.h"
#include "TestJob.h"
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
    mProjects.clear();
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
    if (!(options.options & NoWall))
        mOptions.defaultArguments.append("-Wall");
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

    {
        IniFile file(mOptions.projectsFile);
        List<ByteArray> makefiles = file.keys("Makefiles");
        int count = makefiles.size();
        for (int i=0; i<count; ++i) {
            bool ok;
            const ByteArray value = file.value("Makefiles", makefiles.at(i));
            const MakefileInformation info = MakefileInformation::fromString(value, &ok);
            if (!ok) {
                error("Can't parse makefile information %s", value.constData());
                return false;
            }
            const Path path = Path::resolved(makefiles.at(i));
            mMakefiles[path] = info;
            mMakefilesWatcher.watch(path);
        }
        List<ByteArray> grtags = file.keys("GRTags");
        count = grtags.size();
        for (int i=0; i<count; ++i) {
            grtag(grtags.at(i));
        }

    }

    mServer->clientConnected().connect(this, &Server::onNewConnection);

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
    const MakefileInformation mi(message->arguments(), message->extraFlags());
    mMakefiles[makefile] = mi;
    writeProjects();
    make(message->makefile(), message->arguments(), message->extraFlags(), conn);
}

void Server::handleGRTagMessage(GRTagsMessage *message, Connection *conn)
{
    if (grtag(message->path()))
        conn->write("Parsing " + message->path());
    conn->finish();
}

bool Server::grtag(const Path &dir)
{
    shared_ptr<Project> &project = mProjects[dir];
    if (!project)
        project.reset(new Project(dir));
    if (project->grtags)
        return false;
    if (!project->fileManager) {
        project->fileManager.reset(new FileManager);
        project->fileManager->init(project);
    }
    project->grtags.reset(new GRTags);
    project->grtags->init(project);
    mGRTagsDirs.insert(dir);
    writeProjects();
    setCurrentProject(project);
    return true;
}

void Server::make(const Path &path, const List<ByteArray> &makefileArgs,
                  const List<ByteArray> &extraFlags, Connection *conn)
{
    shared_ptr<Project> project = mProjects.value(path);
    if (project) {
        assert(project->indexer);
        project->indexer->beginMakefile();
    }

    MakefileParser *parser = new MakefileParser(extraFlags, conn);
    parser->fileReady().connect(this, &Server::onFileReady);
    parser->done().connect(this, &Server::onMakefileParserDone);
    parser->run(path, makefileArgs);
}

void Server::onMakefileParserDone(MakefileParser *parser)
{
    assert(parser);
    Connection *connection = parser->connection();
    shared_ptr<Project> project = mProjects.value(parser->makefile());
    if (connection) {
        char buf[1024];
        snprintf(buf, sizeof(buf), "Parsed %s, %d sources",
                 parser->makefile().constData(), parser->sourceCount());
        ResponseMessage msg(buf);
        connection->send(&msg);
        connection->finish();
    }
    if (project) {
        assert(project->indexer);
        project->indexer->endMakefile();
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
    case QueryMessage::DeleteProject: {
        RegExp rx(message->query());
        Set<Path> remove;
        for (Map<Path, shared_ptr<Project> >::iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            if (rx.indexIn(it->first) != -1)
                remove.insert(it->first);
        }

        for (Set<Path>::const_iterator it = remove.begin(); it != remove.end(); ++it) {
            ResponseMessage msg("Erased project: " + *it);
            conn->send(&msg);
            removeProject(*it);
        }
        break; }
    case QueryMessage::Project:
        if (message->query().isEmpty()) {
            shared_ptr<Project> current = currentProject();
            for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                ByteArray b = it->first;
                if (it->second == current)
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
        reindex(*message, conn);
        return; }
    case QueryMessage::ClearProjects: {
        clearProjects();
        ResponseMessage msg("Cleared projects");
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

int Server::findFile(const QueryMessage &query)
{
    error("rc -P %s", query.query().constData());
    shared_ptr<Project> project = currentProject();
    if (!project || !project->fileManager) {
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
    mProjects.clear();
    writeProjects();
}

void Server::reindex(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        conn->write("No project");
        return;
    }

    const int count = project->indexer->reindex(query.query(), query.flags() & QueryMessage::MatchRegexp);
    error() << count << query.query();
    if (count) {
        char buf[128];
        snprintf(buf, sizeof(buf), "Dirtied %d files", count);
        conn->write(buf);
    } else {
        conn->write("No matches");
    }
    conn->finish();
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
    const List<Path> inputFiles = args.inputFiles();
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
    const Path makefile = parser->makefile();
    shared_ptr<Project> &project = mProjects[makefile];
    if (!project) {
        Path srcRoot = findProjectRoot(*args.unresolvedInputFiles().begin());
        if (srcRoot.isEmpty())
            srcRoot = findProjectRoot(*inputFiles.begin());
        if (srcRoot.isEmpty()) {
            mProjects.remove(makefile);
            error("Can't find project root for %s", inputFiles.begin()->constData());
            parser->stop();
            return;
        }
        project.reset(new Project(srcRoot));
        project->indexer.reset(new Indexer(project, !(mOptions.options & NoValidate)));
        project->indexer->beginMakefile();
        project->fileManager.reset(new FileManager);
        project->fileManager->init(project);
    }
    setCurrentProject(project);

    List<ByteArray> arguments = args.clangArgs();
    arguments.append(mOptions.defaultArguments);

    for (int i=0; i<c; ++i) {
        const Path &input = inputFiles.at(i);
        if (project->indexer->compileArguments(Location::insertFile(input)) != arguments) {
            project->indexer->index(input, arguments, IndexerJob::Makefile);
        } else {
            debug() << input << " is not dirty. ignoring";
        }
    }
}

void Server::onMakefileModified(const Path &path)
{
    remake(path, 0);
}

void Server::event(const Event *event)
{
    switch (event->type()) {
    case JobOutputEvent::Type: {
        const JobOutputEvent *e = static_cast<const JobOutputEvent*>(event);
        Map<int, Connection*>::iterator it = mPendingLookups.find(e->job->id());
        if (it == mPendingLookups.end()) {
            break;
        }
        ResponseMessage msg(e->out);
        if (it->second->isConnected()) {
            if (!it->second->send(&msg)) {
                e->job->abort();
            } else if (e->finish) {
                it->second->finish();
            }
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

bool Server::updateProjectForLocation(const Location &location)
{
    const Path path = location.path();
    for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (!strncmp(it->second->srcRoot.constData(), path.constData(), it->second->srcRoot.size())) {
            setCurrentProject(it->second);
            return true;
        }

    }
    return false;
}

shared_ptr<Project> Server::setCurrentProject(const shared_ptr<Project> &proj)
{
    shared_ptr<Project> old = mCurrentProject.lock();
    mCurrentProject = proj;
    return old;
}

void Server::writeProjects()
{
    IniFile ini(mOptions.projectsFile);
    ini.removeGroup("Makefiles");
    mMakefilesWatcher.clear();
    for (Map<Path, MakefileInformation>::const_iterator it = mMakefiles.begin(); it != mMakefiles.end(); ++it) {
        ini.setValue("Makefiles", it->first, it->second.toString());
        mMakefilesWatcher.watch(it->first);
    }
    for (Set<Path>::const_iterator it = mGRTagsDirs.begin(); it != mGRTagsDirs.end(); ++it) {
        ini.setValue("GRTags", *it);
    }
}

void Server::removeProject(const Path &path)
{
    Map<Path, shared_ptr<Project> >::iterator it = mProjects.find(path);
    if (it == mProjects.end())
        return;
    bool write = false;
    if (mMakefiles.remove(path))
        write = true;
    if (mGRTagsDirs.remove(path))
        write = true;
    if (write)
        writeProjects();

    mProjects.remove(path);

    if (!mCurrentProject.lock() && !mProjects.isEmpty())
        setCurrentProject(mProjects.begin()->first);
}
