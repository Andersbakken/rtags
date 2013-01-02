#include "Server.h"

#include "Client.h"
#include "CompletionJob.h"
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
#include "IniFile.h"
#include "ListSymbolsJob.h"
#include "LocalClient.h"
#include "LocalServer.h"
#include "Log.h"
#include "LogObject.h"
#include "MakefileParser.h"
#include "Match.h"
#include "Message.h"
#include "Messages.h"
#include "Path.h"
#include "Preprocessor.h"
#include "Process.h"
#include "ProjectMessage.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include "RegExp.h"
#include "SHA256.h"
#include "StatusJob.h"
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
    : mServer(0), mVerbose(false), mJobId(0), mIndexerThreadPool(0), mQueryThreadPool(2),
      mRestoreProjects(false)
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
    if (mIndexerThreadPool) {
        mIndexerThreadPool->clearBackLog();
        delete mIndexerThreadPool;
        mIndexerThreadPool = 0;
    }
    Path::rm(mOptions.socketFile);
    delete mServer;
    mServer = 0;
    mProjects.clear();
    mCommandProjects.clear();
}

bool Server::init(const Options &options)
{
    mIndexerThreadPool = new ThreadPool(options.threadCount);

    mMakefilesWatcher.modified().connect(this, &Server::onMakefileModified);
    // mMakefilesWatcher.removed().connect(this, &Server::onMakefileRemoved);

    mOptions = options;
    if (!(options.options & NoClangIncludePath)) {
        Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
        clangPath.prepend("-I");
        mOptions.defaultArguments.append(clangPath);
    }
    if (!options.options & NoUnlimitedErrors)
        mOptions.defaultArguments.append("-ferror-limit=0");
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
            Client client(mOptions.socketFile, Client::DontWarnOnConnectionFailure | Client::DontInitMessages);
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

    restore();
    mServer->clientConnected().connect(this, &Server::onNewConnection);
    reloadProjects();

    return true;
}

class CommandProcess : public Process
{
public:
    CommandProcess(unsigned type)
        : mType(type)
    {
        finished().connect(this, &CommandProcess::onFinished);
    }
    void onFinished()
    {
        const ByteArray err = readAllStdErr();
        if (!err.isEmpty()) {
            error() << "Got error" << err;
        }
        const List<ByteArray> lines = readAllStdOut().split('\n');
        // error()  << lines;
        Server *server = Server::instance();
        if (server) {
            const Server::ProjectEntry e(mType | RTags::Type_Synthesized);
            for (int i=0; i<lines.size(); ++i) {
                const Path path = lines.at(i);
                if (path.exists()) {
                    server->addProject(path, e);
                }
            }
        }
        deleteLater();
    }
private:
    const unsigned mType;
};

bool Server::addProject(const Path &p, const ProjectEntry &newEntry)
{
    if (newEntry.type & RTags::Type_Command) {
        mCommandProjects[p] = newEntry;
        const unsigned type = newEntry.type & (RTags::Type_Makefile|RTags::Type_SmartProject);
        CommandProcess *proc = new CommandProcess(type);
        // error() << "Running" << p << ByteArray::join(newEntry.args, ' ');
        proc->start(p, newEntry.args);
        // error() << "Calling start" << p << newEntry.args;
        return true;
    }
    Path path = p;
    if (newEntry.type & RTags::Type_Makefile && path.isDir()) {
        path = Path::resolved("Makefile", path);
        if (!path.isFile()) {
            error() << path << "is not a Makefile";
            return false;
        }
    }
    ProjectEntry &entry = mProjects[path];
    if (!entry.project || newEntry != entry) {
        if (entry.project)
            unloadProject(path);
        entry = newEntry;
        entry.saveKey = p;
        entry.project.reset(new Project(path));
        RTags::encodePath(path);
        const Path cacheFilePath = ByteArray::snprintf<128>("%s%s", mOptions.dataDir.constData(), path.constData());
        if (FILE *f = fopen(cacheFilePath.constData(), "r")) {
            Deserializer in(f);
            int version;
            in >> version;
            if (version == DatabaseVersion) {
                int fs;
                in >> fs;
                if (fs == RTags::fileSize(f)) {
                    Path srcRoot;
                    in >> srcRoot;
                    if (srcRoot.isDir()) {
                        entry.project->setSrcRoot(srcRoot);
                    }
                }
            }
            fclose(f);
        }
        return true;
    }
    return false;
}

void Server::reloadProjects()
{
    mProjects.clear(); // ### could keep the ones that persist somehow
    mCommandProjects.clear();
    mMakefilesWatcher.clear();
    IniFile file(mOptions.projectsFile);
    const Path resolvePath = mOptions.projectsFile.parentDir();
    struct Entry {
        const unsigned type;
        const char *key;
    } entries[] = {
        { RTags::Type_Makefile, "Makefiles" },
        { RTags::Type_SmartProject, "SmartProjects" },
        { RTags::Type_Command|RTags::Type_Makefile, "MakefileCommands" },
        { RTags::Type_Command|RTags::Type_SmartProject, "SmartProjectCommands" },
        { 0, 0 }
    };
    const Path home = Path::home();
    for (int i=0; entries[i].key; ++i) {
        const Entry &e = entries[i];
        const List<ByteArray> keys = file.keys(e.key);
        const int count = keys.size();
        for (int j=0; j<count; ++j) {
            ProjectEntry entry;
            entry.type = e.type;
            ByteArray value = file.value(e.key, keys.at(j));
            Path path = entry.saveKey = keys.at(j);
            if (value.isEmpty() && keys.at(j).contains(' ')) {
                const int space = keys.at(j).indexOf(' ');
                path = keys.at(j).left(space);
                value = keys.at(j).mid(space + 1);
            } else {
                path = keys.at(j);
            }
            entry.saveKey = path;
            if (!value.isEmpty()) {
                const List<ByteArray> split = value.split('|');
                switch (split.size()) {
                case 1:
                    entry.args = value.split(' ');
                    break;
                case 2:
                    entry.args = split.first().split(' ');
                    entry.flags = split.last().split(' ');
                    break;
                default:
                    error("Parse error for %s=%s", keys.at(j).constData(), value.constData());
                    continue;
                }
            }
            if (path.startsWith("$HOME"))
                path.replace(0, 5, home);
            if (path.startsWith('~'))
                path.replace(0, 1, home);
            path = Path::resolved(path, resolvePath);
            addProject(path, entry);
        }
    }
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
    if (!raw.isEmpty()) {
        if (!isCompletionStream(connection)) {
            error() << raw;
        } else {
            warning() << raw;
        }
    }

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
    case CompletionMessage::MessageId: {
        CompletionMessage *completionMessage = static_cast<CompletionMessage*>(message);
        if (completionMessage->flags() & CompletionMessage::Stream) {
            handleCompletionStream(completionMessage, connection);
        } else {
            handleCompletionMessage(static_cast<CompletionMessage*>(message), connection);
        }
        break; }
    case ResponseMessage::MessageId:
        assert(0);
        connection->finish();
        break;
    default:
        error("Unknown message: %d", message->messageId());
        connection->finish();
        break;
    }
}

void Server::handleProjectMessage(ProjectMessage *message, Connection *conn)
{
    switch (message->type()) {
    case RTags::Type_None:
    case RTags::Type_Synthesized:
        assert(0);
        break;
    case RTags::Type_Command:
        break;
    case RTags::Type_Makefile:
    case RTags::Type_SmartProject: {
        ProjectEntry entry;
        entry.type = message->type();
        const Path path = message->path();
        List<ByteArray> args = message->arguments();
        if (message->flags() & ProjectMessage::UseDashB)
            args.append("-B");
        entry.args = args;
        entry.flags = message->extraCompilerFlags();
        if (addProject(path, entry)) {
            conn->write<128>("Added project %s", path.constData());
        } else {
            conn->write<128>("%s is already added", path.constData());
        }
        ProjectEntry &e = mProjects[path];
        bool finish = true;
        if (e.type & RTags::Type_Makefile) {
            if ((e.project && e.project->isValid()) || (message->flags() & ProjectMessage::Automake)) {
                finish = !make(path, args, message->extraCompilerFlags(), conn);
            }
        }
        if (finish)
            conn->finish();
        writeProjects();
        break; }
    }
}

bool Server::make(const Path &path, const List<ByteArray> &makefileArgs,
                  const List<ByteArray> &extraCompilerFlags, Connection *conn)
{
    ProjectEntry entry = mProjects.value(path);
    if (!entry.project || !(entry.type & RTags::Type_Makefile))
        return false;
    if (entry.project && entry.project->isValid()) {
        assert(entry.project->indexer);
        entry.project->indexer->beginMakefile();
        mCurrentProject = entry.project;
    }

    MakefileParser *parser = new MakefileParser(extraCompilerFlags, conn);
    parser->fileReady().connect(this, &Server::onFileReady);
    parser->done().connect(this, &Server::onMakefileParserDone);
    parser->run(path, makefileArgs);
    return true;
}

void Server::onMakefileParserDone(MakefileParser *parser)
{
    assert(parser);
    Connection *connection = parser->connection();
    shared_ptr<Project> project = mProjects.value(parser->makefile()).project;
    int sourceCount = 0;
    if (project && project->indexer) {
        sourceCount = project->indexer->endMakefile();
    }
    if (connection) {
        connection->write<64>("Parsed %s, %d sources",
                              parser->makefile().constData(), sourceCount);
        connection->finish();
    }

    EventLoop::instance()->postEvent(this, new MakefileParserDoneEvent(parser));
}

void Server::handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn)
{
    LogObject *obj = new LogObject(conn, message->level());
    if (message->level() == CompilationError) {
        shared_ptr<Project> project = currentProject();
        if (project && project->indexer) {
            const ByteArray errors = project->indexer->diagnostics();
            if (!errors.isEmpty()) {
                obj->log(errors.constData(), errors.size());
            }
        }
    }
}

void Server::handleQueryMessage(QueryMessage *message, Connection *conn)
{
    conn->setSilent(message->flags() & QueryMessage::Silent);
    updateProject(message->projects());

    switch (message->type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::JobCount:
        jobCount(*message, conn);
        break;
    case QueryMessage::FixIts:
        fixIts(*message, conn);
        break;
    case QueryMessage::FindFile:
        findFile(*message, conn);
        break;
    case QueryMessage::DumpFile:
        dumpFile(*message, conn);
        break;
    case QueryMessage::DeleteProject:
        removeProject(*message, conn);
        break;
    case QueryMessage::UnloadProject:
        removeProject(*message, conn);
        break;
    case QueryMessage::ReloadProjects:
        reloadProjects(*message, conn);
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
    case QueryMessage::PreprocessFile:
        preprocessFile(*message, conn);
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
        conn->write("Not indexed");
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

    FollowLocationJob job(loc, query, project);
    job.run(conn);
    conn->finish();
}

void Server::findFile(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->fileManager) {
        error("No project");
        conn->finish();
        return;
    }

    FindFileJob job(query, project);
    job.run(conn);
    conn->finish();
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
    const SourceInformation c = project->indexer->sourceInfo(fileId);
    if (c.args.isEmpty()) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }

    shared_ptr<IndexerJob> job(new IndexerJob(query, project, c.sourceFile, c.args));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startQueryJob(job);
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

    CursorInfoJob job(loc, query, project);
    job.run(conn);
    conn->finish();
}

void Server::fixIts(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    updateProjectForLocation(path);
    shared_ptr<Project> project = currentProject();
    if (project && project->indexer) {
        ByteArray out = project->indexer->fixIts(Location::fileId(path));
        if (!out.isEmpty())
            conn->write(out);
    }
    conn->finish();
}

void Server::referencesForLocation(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->write("Not indexed");
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

    ReferencesJob job(loc, query, project);
    job.run(conn);
    conn->finish();
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

    ReferencesJob job(name, query, project);
    job.run(conn);
    conn->finish();
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

    FindSymbolsJob job(query, project);
    job.run(conn);
    conn->finish();
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

    ListSymbolsJob job(query, project);
    job.run(conn);
    conn->finish();
}

void Server::status(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    StatusJob job(query, project);
    job.run(conn);
    conn->finish();
}

void Server::isIndexed(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    if (path.isFile()) {
        const uint32_t fileId = Location::fileId(path);
        if (fileId) {
            shared_ptr<Project> old = mCurrentProject.lock();
            updateProjectForLocation(path);
            shared_ptr<Project> cur = currentProject();
            if (cur && cur->isIndexed(fileId)) {
                conn->write("1");
                conn->finish();
                if (old)
                    mCurrentProject = old;
                return;
            }
            if (old)
                mCurrentProject = old;
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
    if (cur && cur->fileManager && (cur->fileManager->contains(path) || cur->match(path))) {
        conn->write("1");
    } else {
        conn->write("0");
    }
    conn->finish();
}

void Server::preprocessFile(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    updateProjectForLocation(path);
    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        conn->write("No project");
        conn->finish();
        return;
    }

    const uint32_t fileId = Location::fileId(path);
    const SourceInformation c = project->indexer->sourceInfo(fileId);
    if (c.args.isEmpty()) {
        conn->write("No arguments for " + path);
        conn->finish();
        return;
    }
    Preprocessor* pre = new Preprocessor(c, conn);
    pre->preprocess();
}

void Server::clearProjects()
{
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it)
        it->second.project->unload();
    RTags::removeDirectory(mOptions.dataDir);
    mCurrentProject.reset();
}

void Server::reindex(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        error("No project");
        conn->finish();
        return;
    }

    const int count = project->indexer->reindex(query.match());
    // error() << count << query.query();
    if (count) {
        conn->write<128>("Dirtied %d files", count);
    } else {
        conn->write("No matches");
    }
    conn->finish();
}

void Server::remake(const Match &match, Connection *conn)
{
    // error() << "remake " << pattern;
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if ((it->second.type & RTags::Type_Makefile) && (match.isEmpty() || match.match(it->first))) {
            make(it->first, it->second.args, it->second.flags, conn);
        }
    }
}

void Server::startIndexerJob(const shared_ptr<IndexerJob> &job, int priority)
{
    mIndexerThreadPool->start(job, priority);
}

void Server::startQueryJob(const shared_ptr<Job> &job)
{
    mQueryThreadPool.start(job);
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
    const int count = inputFiles.size();
    if (!count) {
        warning("no input file?");
        return true;
    } else if (args.lang() == GccArguments::NoLang) {
        return true;
    }
    shared_ptr<Project> project = mProjects.value(proj).project;
    if (!project) {
        error("No project for this file %s %s", inputFiles.begin()->constData(), proj.constData());
        return false;
    }
    if (!project->isValid()) {
        Path srcRoot = findProjectRoot(*args.unresolvedInputFiles().begin());
        if (srcRoot.isEmpty())
            srcRoot = findProjectRoot(*inputFiles.begin());
        if (srcRoot.isEmpty()) {
            error("Can't find project root for %s", inputFiles.begin()->constData());
            return false;
        }
        project->init(srcRoot);
        project->indexer->jobsComplete().connectAsync(this, &Server::onJobsComplete);
        project->indexer->jobStarted().connectAsync(this, &Server::onJobStarted);
        if (mRestoreProjects) {
            Timer timer;
            Path makeFilePath = proj;
            RTags::encodePath(makeFilePath);
            const Path p = ByteArray::snprintf<128>("%s%s", mOptions.dataDir.constData(), makeFilePath.constData());
            if (FILE *f = fopen(p.constData(), "r")) {
                Deserializer in(f);
                int version;
                in >> version;
                if (version == DatabaseVersion) {
                    int fs;
                    in >> fs;
                    if (fs != RTags::fileSize(f)) {
                        error("%s seems to be corrupted, refusing to restore %s",
                              p.constData(), proj.constData());
                    } else {
                        Path srcRoot;
                        in >> srcRoot; // ignored
                        if (!project->restore(in)) {
                            error("Can't restore project %s", proj.constData());
                        } else if (!project->indexer->restore(in)) {
                            error("Can't restore project %s", proj.constData());
                        } else {
                            error("Restored project %s in %dms", proj.constData(), timer.elapsed());
                        }
                    }
                }
                fclose(f);
            }
        }

        project->indexer->beginMakefile();
    }
    if (!mCurrentProject.lock())
        mCurrentProject = project;

    List<ByteArray> arguments = args.clangArgs();
    arguments.append(mOptions.defaultArguments);

    SourceInformation c(Path(), arguments, args.compiler());
    for (int i=0; i<count; ++i) {
        c.sourceFile = inputFiles.at(i);

        const SourceInformation existing = project->indexer->sourceInfo(Location::insertFile(c.sourceFile));
        if (existing != c) {
            project->indexer->index(c, IndexerJob::Makefile);
        } else {
            debug() << c.sourceFile << " is not dirty. ignoring";
        }
    }
    return true;
}

void Server::onMakefileModified(const Path &path)
{
    shared_ptr<Project> project = mProjects.value(path).project;
    if (project && project->indexer)
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

        if (e->finish && !isCompletionStream(it->second))
            it->second->finish();
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
    ProjectsMap::const_iterator it = mProjects.find(path);
    if (it != mProjects.end()) {
        mCurrentProject = it->second.project;
        if (!it->second.project->isValid()) {
            if (it->second.type & RTags::Type_Makefile) {
                remake(it->first, 0);
            } else if (it->second.type & RTags::Type_SmartProject) {
                initSmartProject(it->second);
            }
        }
        return it->second.project;
    }
    return shared_ptr<Project>();
}

bool Server::updateProjectForLocation(const Location &location)
{
    return updateProjectForLocation(location.path());
}

bool Server::updateProjectForLocation(const Path &path)
{
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second.project->match(path)) {
            setCurrentProject(it->second.project->path());
            return true;
        }
    }
    return false;
}

void Server::writeProjects()
{
    Path::rm(mOptions.projectsFile);
    IniFile ini(mOptions.projectsFile);
    mMakefilesWatcher.clear();
    const ProjectsMap *maps[] = { &mProjects, &mCommandProjects };
    for (int i=0; i<2; ++i) {
        const ProjectsMap &map = *maps[i];
        for (ProjectsMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            if (it->second.type & RTags::Type_Makefile && !(it->second.type & RTags::Type_Command))
                mMakefilesWatcher.watch(it->first);
            if (it->second.type & RTags::Type_Synthesized)
                continue;
            const char *key = 0;
            switch (it->second.type) {
            case RTags::Type_Makefile: key = "Makefiles"; break;
            case RTags::Type_SmartProject: key = "SmartProjects"; break;
            case RTags::Type_Command|RTags::Type_Makefile: key = "MakefileCommands"; break;
            case RTags::Type_Command|RTags::Type_SmartProject: key = "SmartProjectCommands"; break;
            default:
                assert(0);
            }
            assert(key);
            if (key) {
                ByteArray details;
                if (!it->second.args.isEmpty())
                    details = ByteArray::join(it->second.args, ' ');
                if (!it->second.flags.isEmpty()) {
                    details += '|';
                    details += ByteArray::join(it->second.flags, ' ');
                }

                ini.setValue(key, it->second.saveKey, details);
            }
        }
    }
}

void Server::removeProject(const Path &path)
{
    bool ok;
    ProjectEntry entry = mProjects.take(path, &ok);
    if (!ok)
        entry = mCommandProjects.take(path, &ok);
    if (!ok)
        return;
    if (entry.project)
        entry.project->unload();
    const bool write = !(entry.type & RTags::Type_Synthesized);
    if (write)
        writeProjects();
}

void Server::unloadProject(const Path &path)
{
    ProjectsMap::iterator it = mProjects.find(path);
    if (it == mProjects.end())
        return;
    it->second.project->unload();
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

struct SmartProjectFileUserData {
    List<RegExp> includes, excludes;
    List<ByteArray> includesWildcard, excludesWildcard;
    List<Path> sources;
    Set<Path> includePaths;
    bool recurse;
};

static Path::VisitResult findHeaderVisitor(const Path &path, void *userData)
{
    if (path.isHeader()) {
        *reinterpret_cast<bool*>(userData) = true;
        return Path::Abort;
    }
    return Path::Continue;
}

static Path::VisitResult smartProjectFileVisitor(const Path &path, void *userData)
{
    static const int max = strtoul("RTAGS_SMART_PROJECT_MAX", 0, 10);
    assert(userData);
    SmartProjectFileUserData &ud = *reinterpret_cast<SmartProjectFileUserData*>(userData);
    switch (path.type()) {
    case Path::File:
        if ((!max || ud.sources.size() < max)
            && match(path, ud.includesWildcard, ud.includes)
            && !match(path, ud.excludesWildcard, ud.excludes)) {
            ud.sources.append(path);
        }
        break;
    case Path::Directory: {
        bool hasHeaders = false;
        path.visit(findHeaderVisitor, &hasHeaders);
        if (hasHeaders)
            ud.includePaths.insert(path);
        if (ud.recurse)
            return Path::Recurse;
        break; }
    default:
        break;
    }
    return Path::Continue;
}

bool Server::initSmartProject(const ProjectEntry &entry)
{
    if (entry.project->isValid())
        return true;
    const Path path = entry.project->path();
    Map<Path, SmartProjectFileUserData> dirs;
    switch (path.type()) {
    case Path::File:
        break;
    case Path::Directory: {
        SmartProjectFileUserData &data = dirs[path];
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
    entry.project->init(path);
    entry.project->indexer->jobsComplete().connectAsync(this, &Server::onJobsComplete);
    entry.project->indexer->jobStarted().connectAsync(this, &Server::onJobStarted);
    entry.project->indexer->beginMakefile();
    for (Map<Path, SmartProjectFileUserData>::iterator it = dirs.begin(); it != dirs.end(); ++it) {
        SmartProjectFileUserData &ud = it->second;
        ud.includePaths.insert(path); // ###
        it->first.visit(smartProjectFileVisitor, &ud);
        GccArguments args;
        args.mInputFiles = ud.sources;
        const char *suffix = path.extension();
        if (suffix && !strcmp(suffix, "c")) {
            args.mLang = GccArguments::C;
        } else {
            args.mLang = GccArguments::CPlusPlus;
        }

        for (Set<Path>::const_iterator it = ud.includePaths.begin(); it != ud.includePaths.end(); ++it) {
            args.mClangArgs.append("-I" + *it);
        }
        args.mClangArgs += entry.flags;
        processSourceFile(args, path);
    }
    entry.project->indexer->endMakefile();

    // error() << userData.includePaths;
    // error() << userData.sources;
    return true;
}
void Server::removeProject(const QueryMessage &query, Connection *conn)
{
    const Match match = query.match();
    Set<Path> remove;
    for (ProjectsMap::iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second.project->match(match))
            remove.insert(it->first);
    }
    const bool unload = query.type() == QueryMessage::UnloadProject;

    for (Set<Path>::const_iterator it = remove.begin(); it != remove.end(); ++it) {
        Path path = *it;
        conn->write<128>("%s project: %s", unload ? "Unloaded" : "Deleted", path.constData());
        if (!unload) {
            RTags::encodePath(path);
            Path::rm(mOptions.dataDir + path);
            removeProject(*it);
        } else {
            unloadProject(*it);
        }
    }
    conn->finish();
}

void Server::reloadProjects(const QueryMessage &query, Connection *conn)
{
    const int old = mProjects.size();
    reloadProjects();
    const int cur = mProjects.size();
    conn->write<128>("Changed from %d to %d projects", old, cur);
    conn->finish();
}

bool Server::selectProject(const Match &match, Connection *conn)
{
    Path selected;
    bool error = false;
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second.project->match(match)) {
            if (error) {
                if (conn)
                    conn->write(it->first);
            } else if (!selected.isEmpty()) {
                error = true;
                if (conn) {
                    conn->write<128>("Multiple matches for %s", match.pattern().constData());
                    conn->write(selected);
                    conn->write(it->first);
                }
                selected.clear();
            } else {
                selected = it->first;
            }
        }
    }
    if (!selected.isEmpty()) {
        shared_ptr<Project> current = currentProject();
        if (!current || selected != current->path()) {
            setCurrentProject(selected);
            if (conn)
                conn->write<128>("Selected project: %s for %s", selected.constData(), match.pattern().constData());
        }
        return true;
    } else if (!error && conn) {
        conn->write<128>("No matches for %s", match.pattern().constData());
    }
    return false;
}

bool Server::updateProject(const List<ByteArray> &projects)
{
    for (int i=0; i<projects.size(); ++i) {
        if (selectProject(projects.at(i), 0))
            return true;
    }
    return false;
}

void Server::project(const QueryMessage &query, Connection *conn)
{
    if (query.query().isEmpty()) {
        shared_ptr<Project> current = currentProject();
        for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            Path srcRoot = it->second.project->srcRoot();
            if (!srcRoot.isEmpty())
                srcRoot.prepend(" src: ");
            conn->write<128>("%s%s%s%s",
                             it->first.constData(),
                             srcRoot.constData(),
                             it->second.project->isValid() ? " (loaded)" : "",
                             it->second.project == current ? " <=" : "");
        }
    } else {
        Path selected;
        bool error = false;
        const Match match = query.match();
        const ProjectsMap::const_iterator it = mProjects.find(match.pattern());
        if (it != mProjects.end() && it->second.project) {
            selected = it->first;
        } else {
            for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                if (it->second.project && it->second.project->match(match)) {
                    if (error) {
                        conn->write(it->first);
                    } else if (!selected.isEmpty()) {
                        error = true;
                        conn->write<128>("Multiple matches for %s", match.pattern().constData());
                        conn->write(selected);
                        conn->write(it->first);
                        selected.clear();
                    } else {
                        selected = it->first;
                    }
                }
            }
        }
        if (!selected.isEmpty()) {
            shared_ptr<Project> current = currentProject();
            if (!current || selected != current->path()) {
                setCurrentProject(selected);
                conn->write<128>("Selected project: %s for %s", selected.constData(), match.pattern().constData());
            }
        } else if (!error) {
            conn->write<128>("No matches for %s", match.pattern().constData());
        }
    }
    conn->finish();
}

void Server::jobCount(const QueryMessage &query, Connection *conn)
{
    if (query.query().isEmpty()) {
        conn->write<128>("Running with %d jobs", mOptions.threadCount);
    } else {
        const int jobCount = query.query().toLongLong();
        if (jobCount <= 0 || jobCount > 100) {
            conn->write<128>("Invalid job count %s (%d)", query.query().constData(), jobCount);
        } else {
            mOptions.threadCount = jobCount;
            mIndexerThreadPool->setConcurrentJobs(jobCount);
            conn->write<128>("Changed jobs to %d", jobCount);
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

void Server::handleCompletionMessage(CompletionMessage *message, Connection *conn)
{
    updateProject(message->projects());
    const Path path = message->path();
    updateProjectForLocation(path);

    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        if (!isCompletionStream(conn))
            conn->finish();
        return;
    }
    if (mActiveCompletions.contains(path)) {
        PendingCompletion &pending = mPendingCompletions[path];
        pending.line = message->line();
        pending.column = message->column();
        pending.pos = message->pos();
        pending.contents = message->contents();
        pending.connection = conn;
    } else {
        startCompletion(path, message->line(), message->column(), message->pos(), message->contents(), conn);
    }
}

void Server::startCompletion(const Path &path, int line, int column, int pos, const ByteArray &contents, Connection *conn)
{
    // error() << "starting completion" << path << line << column;
    updateProjectForLocation(path);

    shared_ptr<Project> project = currentProject();
    if (!project || !project->indexer) {
        if (!isCompletionStream(conn))
            conn->finish();
        return;
    }
    const uint32_t fileId = Location::fileId(path);
    if (!fileId)
        return;

    CXIndex index;
    CXTranslationUnit unit;
    List<ByteArray> args;
    if (!project->indexer->fetchFromCache(path, args, index, unit)) {
        const SourceInformation info = project->indexer->sourceInfo(fileId);
        if (!info.isNull()) {
            project->indexer->reindex(path);
            conn->write<128>("Scheduled rebuild of %s", path.constData());
        }
        if (!isCompletionStream(conn))
            conn->finish();
        return;
    }

    mActiveCompletions.insert(path);
    shared_ptr<CompletionJob> job(new CompletionJob(project));
    job->init(index, unit, path, args, line, column, pos, contents);
    job->setId(nextId());
    job->finished().connectAsync(this, &Server::onCompletionJobFinished);
    mPendingLookups[job->id()] = conn;
    startQueryJob(job);
}

void Server::onCompletionJobFinished(Path path)
{
    // error() << "Got finished for" << path;
    PendingCompletion completion = mPendingCompletions.take(path);
    if (completion.line != -1) {
        startCompletion(path, completion.line, completion.column, completion.pos, completion.contents, completion.connection);
        // ### could the connection be deleted by now?
    } else {
        mActiveCompletions.remove(path);
    }
}

bool Server::isCompletionStream(Connection* conn) const
{
    LocalClient *client = conn->client();
    return (mCompletionStreams.find(client) != mCompletionStreams.end());
}

void Server::onCompletionStreamDisconnected(LocalClient *client)
{
    mCompletionStreams.remove(client);
}

void Server::handleCompletionStream(CompletionMessage *message, Connection *conn)
{
    LocalClient *client = conn->client();
    assert(client);
    client->disconnected().connect(this, &Server::onCompletionStreamDisconnected);
    mCompletionStreams[client] = conn;
}

void Server::save(const shared_ptr<Indexer> &indexer)
{
    if (!Path::mkdir(mOptions.dataDir)) {
        error("Can't create directory [%s]", mOptions.dataDir.constData());
        return;
    }
    {
        const Path p = mOptions.dataDir + "fileids";
        FILE *f = fopen(p.constData(), "w");
        if (!f) {
            error("Can't open file %s", p.constData());
            return;
        }
        const Map<Path, uint32_t> pathsToIds = Location::pathsToIds();
        Serializer out(f);
        out << static_cast<int>(DatabaseVersion);
        const int pos = ftell(f);
        out << static_cast<int>(0) << pathsToIds;
        const int size = ftell(f);
        fseek(f, pos, SEEK_SET);
        out << size;
        fclose(f);
    }
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second.project->indexer != indexer)
            continue;
        Timer timer;
        Path makeFilePath = it->first;
        RTags::encodePath(makeFilePath);
        const Path p = mOptions.dataDir + makeFilePath;
        FILE *f = fopen(p.constData(), "w");
        if (!f) {
            error("Can't open file %s", p.constData());
            return;
        }
        Serializer out(f);
        out << static_cast<int>(DatabaseVersion);
        const int pos = ftell(f);
        out << static_cast<int>(0);
        out << it->second.project->srcRoot();
        if (!it->second.project->save(out)) {
            error("Can't save project %s", it->first.constData());
            fclose(f);
            return;
        }
        if (!it->second.project->indexer->save(out)) {
            error("Can't save project %s", it->first.constData());
            fclose(f);
            return;
        }
        const int size = ftell(f);
        fseek(f, pos, SEEK_SET);
        out << size;

        error() << "saved project" << it->first << "in" << ByteArray::snprintf<12>("%dms", timer.elapsed()).constData();
        fclose(f);
        break;
    }
}

void Server::onJobsComplete(shared_ptr<Indexer> indexer, int count)
{
    bool ok;
    const int id = mSaveTimers.take(indexer, &ok);
    if (ok && id != -1)
        EventLoop::instance()->removeTimer(id);
    if (count) {
        enum { SaveTimerInterval = 1000 };
        mSaveTimers[indexer] = EventLoop::instance()->addTimer(SaveTimerInterval, Server::saveTimerCallback,
                                                               new shared_ptr<Indexer>(indexer));
    }
}

void Server::saveTimerCallback(int id, void *userData)
{
    shared_ptr<Indexer> *indexer = static_cast<shared_ptr<Indexer> *>(userData);
    EventLoop::instance()->removeTimer(id);
    Server::instance()->save(*indexer);
    delete indexer;
    // ### should maybe not do this in the main thread
}

void Server::onJobStarted(shared_ptr<Indexer> indexer, Path path)
{
    // error() << path.constData() << "started";
    bool ok;
    const int id = mSaveTimers.take(indexer, &ok);
    if (ok && id != -1)
        EventLoop::instance()->removeTimer(id);
}

void Server::restore()
{
    const Path p = mOptions.dataDir + "fileids";
    FILE *f = fopen(p.constData(), "r");
    if (!f)
        return;
    Map<Path, uint32_t> pathsToIds;
    Deserializer in(f);
    int version;
    in >> version;
    if (version == DatabaseVersion) {
        int size;
        in >> size;
        if (size != RTags::fileSize(f)) {
            error("Refusing to load corrupted file %s", p.constData());
        } else {
            in >> pathsToIds;
            Location::init(pathsToIds);
            mRestoreProjects = true;
        }
        fclose(f);
    }
}
