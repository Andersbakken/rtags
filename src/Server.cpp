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
#include "Indexer.h"
#include "IndexerJob.h"
#include "IniFile.h"
#include "ListSymbolsJob.h"
#include "LocalClient.h"
#include "LocalServer.h"
#include "Log.h"
#include "LogObject.h"
#include "MakefileInformation.h"
#include "MakefileParser.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "ProjectMessage.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include "RegExp.h"
#include "SHA256.h"
#include "StatusJob.h"
#include "TestJob.h"
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
        const Path resolvePath = mOptions.projectsFile.parentDir();
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
            const Path path = Path::resolved(makefiles.at(i), resolvePath);
            mMakefiles[path] = info;
            mMakefilesWatcher.watch(path);
        }
        List<ByteArray> grtags = file.keys("GRTags");
        count = grtags.size();
        for (int i=0; i<count; ++i) {
            grtag(grtags.at(i));
        }

        List<ByteArray> smartProjects = file.keys("SmartProjects");
        count = smartProjects.size();
        for (int i=0; i<count; ++i) {
            const ByteArray value = file.value("SmartProjects", smartProjects.at(i));
            smartProject(smartProjects.at(i), value.split('|'));
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

void Server::onNewMessage(Message *message, Connection *connection)
{
    ClientMessage *m = static_cast<ClientMessage*>(message);
    const ByteArray raw = m->raw();
    if (!raw.isEmpty())
        error() << raw;
    switch (message->messageId()) {
    case ProjectMessage::MessageId:
        handleProjectMessage(static_cast<ProjectMessage*>(message), connection);
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(static_cast<QueryMessage*>(message), connection);
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

void Server::handleProjectMessage(ProjectMessage *message, Connection *conn)
{
    switch (message->type()) {
    case ProjectMessage::NoType:
        break;
    case ProjectMessage::MakefileType: {
        const Path makefile = message->path();
        List<ByteArray> args = message->arguments();
        if (message->flags() & ProjectMessage::UseDashB)
            args.append("-B");
        if (message->flags() & ProjectMessage::NoMakeTricks)
            args.append("<no-make-tricks>");
        const MakefileInformation mi(args, message->extraCompilerFlags());
        mMakefiles[makefile] = mi;
        writeProjects();
        make(makefile, args, message->extraCompilerFlags(), conn);
        break; }
    case ProjectMessage::GRTagsType:
        if (grtag(message->path()))
            conn->write<256>("Parsing %s", message->path().constData());
        conn->finish();
        break;
    case ProjectMessage::SmartType:
        if (smartProject(message->path(), message->extraCompilerFlags()))
            conn->write<256>("Parsing %s", message->path().constData());
        conn->finish();
        break;
    }
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
                  const List<ByteArray> &extraCompilerFlags, Connection *conn)
{
    shared_ptr<Project> project = mProjects.value(path);
    if (project) {
        assert(project->indexer);
        project->indexer->beginMakefile();
    }

    MakefileParser *parser = new MakefileParser(extraCompilerFlags, conn);
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
        connection->write<64>("Parsed %s, %d sources",
                              parser->makefile().constData(), parser->sourceCount());
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
    LogObject *obj = new LogObject(conn, message->level());
    if (message->level() == CompilationError) {
        shared_ptr<Project> project = currentProject();
        if (project && project->indexer) {
            const ByteArray errors = project->indexer->errors();
            if (!errors.isEmpty()) {
                obj->log(errors.constData(), errors.size());
            }
        }
    }
}

void Server::handleQueryMessage(QueryMessage *message, Connection *conn)
{
    if (message->flags() & QueryMessage::Silent)
        conn->setSilent(true);

    switch (message->type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::FindFile:
        findFile(*message, conn);
        break;
    case QueryMessage::DumpFile:
        dumpFile(*message, conn);
        break;
    case QueryMessage::DeleteProject:
        deleteProject(*message, conn);
        break;
    case QueryMessage::Project:
        project(*message, conn);
        break;
    case QueryMessage::Reindex: {
        reindex(*message, conn);
        break; }
    case QueryMessage::ClearProjects:
        clearProjects(*message, conn);
        break;
    case QueryMessage::FixIts:
        fixIts(*message, conn);
        break;
    case QueryMessage::Errors:
        errors(*message, conn);
        break;
    case QueryMessage::CursorInfo:
        cursorInfo(*message, conn);
        break;
    case QueryMessage::Shutdown:
        shutdown(*message, conn);
        break;
    case QueryMessage::FollowLocation:
        followLocation(*message, conn);
        break;
    case QueryMessage::ReferencesLocation:
        referencesForLocation(*message, conn);
        break;
    case QueryMessage::ReferencesName:
        referencesForName(*message, conn);
        break;
    case QueryMessage::ListSymbols:
        listSymbols(*message, conn);
        break;
    case QueryMessage::FindSymbols:
        findSymbols(*message, conn);
        break;
    case QueryMessage::Status:
        status(*message, conn);
        break;
    case QueryMessage::IsIndexed:
        isIndexed(*message, conn);
        break;
    case QueryMessage::HasFileManager:
        hasFileManager(*message, conn);
        break;
    }
}

int Server::nextId()
{
    ++mJobId;
    if (!mJobId)
        ++mJobId;
    return mJobId;
}

void Server::followLocation(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    updateProjectForLocation(loc);

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<FollowLocationJob> job(new FollowLocationJob(loc, query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::findFile(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->fileManager) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<FindFileJob> job(new FindFileJob(query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::dumpFile(const QueryMessage &query, Connection *conn)
{
    const uint32_t fileId = Location::fileId(query.query());
    if (!fileId) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }

    Location loc(fileId, 0);
    updateProjectForLocation(loc);

    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }
    const List<ByteArray> args = project->indexer->compileArguments(fileId);
    if (args.isEmpty()) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }

    shared_ptr<IndexerJob> job(new IndexerJob(query, project, Location::path(fileId), args));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::cursorInfo(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    updateProjectForLocation(loc);

    shared_ptr<Project> project = currentProject();
    if (!project) {
        conn->finish();
        return;
    }

    shared_ptr<CursorInfoJob> job(new CursorInfoJob(loc, query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}


void Server::referencesForLocation(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    updateProjectForLocation(loc);

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<ReferencesJob> job(new ReferencesJob(loc, query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::referencesForName(const QueryMessage& query, Connection *conn)
{
    const ByteArray name = query.query();

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<ReferencesJob> job(new ReferencesJob(name, query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::findSymbols(const QueryMessage &query, Connection *conn)
{
    const ByteArray partial = query.query();

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<FindSymbolsJob> job(new FindSymbolsJob(query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::listSymbols(const QueryMessage &query, Connection *conn)
{
    const ByteArray partial = query.query();

    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<ListSymbolsJob> job(new ListSymbolsJob(query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::status(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    shared_ptr<StatusJob> job(new StatusJob(query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startJob(job);
}

void Server::isIndexed(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    if (path.isFile()) {
        const uint32_t fileId = Location::fileId(path);
        if (fileId) {
            updateProjectForLocation(path);
            shared_ptr<Project> cur = currentProject();
            if (cur && cur->isIndexed(fileId)) {
                conn->write("1");
                conn->finish();
                return;
            }
        }
    } else if (path.isDir()) {
        updateProjectForLocation(path);
        shared_ptr<Project> cur = currentProject();
        if (cur && cur->fileManager->contains(path)) {
            conn->write("1");
            conn->finish();
            return;
        }
    }
    conn->write("0");
    conn->finish();
}

void Server::hasFileManager(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    updateProjectForLocation(path);
    shared_ptr<Project> cur = currentProject();
    if (cur && cur->fileManager->contains(path)) {
        conn->write("1");
    } else {
        conn->write("0");
    }
    conn->finish();
}

void Server::fixIts(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        error("No project");
        conn->finish();
        return;
    }

    const ByteArray fixIts = project->indexer->fixIts(query.query());

    conn->write(fixIts);
    conn->finish();
}

void Server::errors(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        error("No project");
        conn->finish();
        return;
    }

    const ByteArray errors = project->indexer->errors(query.query());

    conn->write(errors);
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
        error("No project");
        conn->finish();
        return;
    }

    const int count = project->indexer->reindex(query.query(), query.flags() & QueryMessage::MatchRegexp);
    // error() << count << query.query();
    if (count) {
        conn->write<128>("Dirtied %d files", count);
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
            make(it->first, it->second.makefileArgs, it->second.extraCompilerFlags, conn);
        }
    }
}

void Server::startJob(const shared_ptr<Job> &job)
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
    if (!processSourceFile(args, parser->makefile()))
        parser->stop();
}

bool Server::processSourceFile(const GccArguments &args, const Path &proj)
{
    const List<Path> inputFiles = args.inputFiles();
    const int c = inputFiles.size();
    if (!c) {
        warning("no input file?");
        return true;
    } else if (args.type() == GccArguments::NoType || args.lang() == GccArguments::NoLang) {
        return true;
    }
    shared_ptr<Project> &project = mProjects[proj];
    if (!project) {
        Path srcRoot = findProjectRoot(*args.unresolvedInputFiles().begin());
        if (srcRoot.isEmpty())
            srcRoot = findProjectRoot(*inputFiles.begin());
        if (srcRoot.isEmpty()) {
            mProjects.remove(proj);
            error("Can't find project root for %s", inputFiles.begin()->constData());
            return false;
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
    return true;
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
        Map<int, Connection*>::iterator it = mPendingLookups.find(e->id);
        if (it == mPendingLookups.end()) {
            if (shared_ptr<Job> job = e->job.lock())
                job->abort();
            break;
        }
        if (!it->second->isConnected()) {
            if (shared_ptr<Job> job = e->job.lock())
                job->abort();
            break;
        }
        if (!e->out.isEmpty() && !it->second->write(e->out)) {
            if (shared_ptr<Job> job = e->job.lock())
                job->abort();
            break;
        }

        if (e->finish) {
            it->second->finish();
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
    return updateProjectForLocation(location.path());
}

bool Server::updateProjectForLocation(const Path &path, Path *key)
{
    shared_ptr<Project> match;
    int longest = -1;
    for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (!strncmp(it->second->srcRoot.constData(), path.constData(), it->second->srcRoot.size())) {
            const int matchLength = it->second->srcRoot.size();
            if (matchLength > longest) {
                match = it->second;
                longest = matchLength;
                if (key)
                    *key = it->first;
            }
        }
        if (!it->second->resolvedSrcRoot.isEmpty()
            && !strncmp(it->second->resolvedSrcRoot.constData(), path.constData(), it->second->resolvedSrcRoot.size())) {
            const int matchLength = it->second->srcRoot.size();
            if (matchLength > longest) {
                match = it->second;
                longest = matchLength;
                if (key)
                    *key = it->first;
            }
        }

    }
    if (match) {
        setCurrentProject(match);
        return true;
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
    ini.removeGroup("GRTags");
    for (Set<Path>::const_iterator it = mGRTagsDirs.begin(); it != mGRTagsDirs.end(); ++it) {
        ini.setValue("GRTags", *it);
    }
    ini.removeGroup("SmartProjects");
    for (Map<Path, List<ByteArray> >::const_iterator it = mSmartProjects.begin(); it != mSmartProjects.end(); ++it) {
        ini.setValue("SmartProjects", it->first, ByteArray::join(it->second, '|'));
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
    if (mSmartProjects.remove(path))
        write = true;
    if (write)
        writeProjects();

    mProjects.remove(path);

    if (!mCurrentProject.lock() && !mProjects.isEmpty())
        setCurrentProject(mProjects.begin()->first);
}

static inline bool match(const Path &path, const List<ByteArray> &wildcards, const List<RegExp> &regexps)
{
    // error() << "matching" << path << "vs" << wildcards; // << regexps;
    const char *p = path.constData();
    for (int i=0; i<wildcards.size(); ++i) {
        if (!fnmatch(wildcards.at(i).constData(), p, 0))
            return true;
    }
    for (int i=0; i<regexps.size(); ++i) {
        if (regexps.at(i).indexIn(path) != -1)
            return true;
    }
    return false;
}

struct ProjectFileUserData {
    List<RegExp> includes, excludes;
    List<ByteArray> includesWildcard, excludesWildcard;
    List<Path> sources;
    Set<Path> includePaths;
    bool recurse;
};

static Path::VisitResult projectFileVisitor(const Path &path, void *userData)
{
    assert(userData);
    ProjectFileUserData &ud = *reinterpret_cast<ProjectFileUserData*>(userData);
    switch (path.type()) {
    case Path::File:
        if (match(path, ud.includesWildcard, ud.includes)
            && !match(path, ud.excludesWildcard, ud.excludes)) {
            ud.sources.append(path);
        }
        break;
    case Path::Directory:
        ud.includePaths.insert(path);
        if (ud.recurse)
            return Path::Recurse;
        break;
    default:
        break;
    }
    return Path::Continue;
}

bool Server::smartProject(const Path &path, const List<ByteArray> &extraCompilerFlags)
{
    if (mProjects.contains(path))
        return false;
    Map<Path, ProjectFileUserData> dirs;
    switch (path.type()) {
    case Path::File:
        break;
    case Path::Directory: {
        ProjectFileUserData &data = dirs[path];
        data.recurse = true;
        data.includesWildcard.append("*.c");
        data.includesWildcard.append("*.cpp");
        data.includesWildcard.append("*.cc");
        data.includesWildcard.append("*.cxx");
        data.includesWildcard.append("*.C");
        break; }
    default:
        break;
    }
    if (dirs.isEmpty())
        return false;
    shared_ptr<Project> &project = mProjects[path];
    project.reset(new Project(path));
    project->indexer.reset(new Indexer(project, !(mOptions.options & NoValidate)));
    project->indexer->beginMakefile();
    project->fileManager.reset(new FileManager);
    project->fileManager->init(project);
    for (Map<Path, ProjectFileUserData>::iterator it = dirs.begin(); it != dirs.end(); ++it) {
        ProjectFileUserData &ud = it->second;
        ud.includePaths.insert(path); // ###
        it->first.visit(projectFileVisitor, &ud);
        GccArguments args;
        args.mInputFiles = ud.sources;
        args.mType = GccArguments::Compile;
        args.mLang = GccArguments::CPlusPlus; // ### lying a little
        for (Set<Path>::const_iterator it = ud.includePaths.begin(); it != ud.includePaths.end(); ++it) {
            args.mClangArgs.append("-I" + *it);
        }
        args.mClangArgs += extraCompilerFlags;
        processSourceFile(args, path);
    }
    project->indexer->endMakefile();

    // error() << userData.includePaths;
    // error() << userData.sources;
    mSmartProjects[path] = extraCompilerFlags;
    writeProjects();
    return true;
}
void Server::deleteProject(const QueryMessage &query, Connection *conn)
{
    RegExp rx(query.query());
    Set<Path> remove;
    for (Map<Path, shared_ptr<Project> >::iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (rx.indexIn(it->first) != -1)
            remove.insert(it->first);
    }

    for (Set<Path>::const_iterator it = remove.begin(); it != remove.end(); ++it) {
        conn->write<128>("Erased project: %s", it->constData());
        removeProject(*it);
    }
    conn->finish();
}
void Server::project(const QueryMessage &query, Connection *conn)
{
    if (query.query().isEmpty()) {
        shared_ptr<Project> current = currentProject();
        for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            conn->write<128>("%s%s",
                             it->first.constData(),
                             it->second == current ? " <=" : "");
        }
    } else {
        shared_ptr<Project> selected;
        bool error = false;
        const Path path = query.query();
        Path key;
        if (path.exists() && updateProjectForLocation(path, &key)) {
            conn->write<128>("Selected project: %s", key.constData());
        } else {
            RegExp rx(query.query());
            for (Map<Path, shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                const Path *paths[] = { &it->first, &it->second->srcRoot, &it->second->resolvedSrcRoot, 0 };
                if (paths[2]->isEmpty())
                    paths[2] = 0;
                for (int i=0; paths[i]; ++i) {
                    if (rx.indexIn(*paths[i]) != -1) {
                        if (error) {
                            conn->write(it->first);
                        } else if (selected) {
                            error = true;
                            conn->write<128>("Multiple matches for %s", query.query().constData());
                            conn->write(it->first);
                            selected.reset();
                        } else {
                            selected = it->second;
                            break;
                        }
                    }
                }
            }
            if (selected) {
                setCurrentProject(selected);
                conn->write<128>("Selected project: %s", selected->srcRoot.constData());
            } else if (!error) {
                conn->write<128>("No matches for %s", query.query().constData());
            }
        }
    }
    conn->finish();
}
void Server::clearProjects(const QueryMessage &query, Connection *conn)
{
    clearProjects();
    conn->write("Cleared projects");
    conn->finish();
}
void Server::shutdown(const QueryMessage &query, Connection *conn)
{
    EventLoop::instance()->exit();
    conn->write("Shutting down");
    conn->finish();
}
