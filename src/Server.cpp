#include "Server.h"

#include "Client.h"
#include "CompileJob.h"
#include "CompileMessage.h"
#include "CompletionJob.h"
#include "Connection.h"
#include "CreateOutputMessage.h"
#include "CursorInfoJob.h"
#include <rct/Event.h>
#include <rct/EventLoop.h>
#include "Filter.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "IndexerJob.h"
#include "JSONJob.h"
#include "ListSymbolsJob.h"
#include "LocalClient.h"
#include "LocalServer.h"
#include <rct/Log.h>
#include "LogObject.h"
#include "Match.h"
#include "Message.h"
#include "Messages.h"
#include <rct/Path.h>
#include "Preprocessor.h"
#include <rct/Process.h>
#include "QueryMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include <rct/RegExp.h>
#include <rct/SHA256.h>
#include "StatusJob.h"
#include <clang-c/Index.h>
#include <stdio.h>

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
    MutexLocker lock(&mMutex);
    if (mIndexerThreadPool) {
        mIndexerThreadPool->clearBackLog();
        delete mIndexerThreadPool;
        mIndexerThreadPool = 0;
    }
    Path::rm(mOptions.socketFile);
    delete mServer;
    mServer = 0;
    mProjects.clear();
}

bool Server::init(const Options &options)
{
    mIndexerThreadPool = new ThreadPool(options.threadCount);

    mOptions = options;
    if (options.options & ClangIncludePath) {
        Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
        clangPath.prepend("-I");
        mOptions.defaultArguments.append(clangPath);
    }
    if (options.options & UnlimitedErrors)
        mOptions.defaultArguments.append("-ferror-limit=0");
    if (options.options & Wall)
        mOptions.defaultArguments.append("-Wall");
    if (options.options & SpellChecking)
        mOptions.defaultArguments << "-fspell-checking";
    mClangPath = Path::resolved(CLANG_BIN "/clang");
    error() << "using args:" << String::join(mOptions.defaultArguments, " ");

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
            enum { Timeout = 5000 };
            Client client(mOptions.socketFile, Timeout, Client::DontWarnOnConnectionFailure | Client::DontInitMessages);
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

    restoreFileIds();
    mServer->clientConnected().connect(this, &Server::onNewConnection);
    reloadProjects();

    return true;
}

shared_ptr<Project> Server::addProject(const Path &path) // lock always held
{
    shared_ptr<Project> &project = mProjects[path];
    if (!project) {
        project.reset(new Project(path));
        return project;
    }
    return shared_ptr<Project>();
}

int Server::reloadProjects()
{
    MutexLocker lock(&mMutex);
    mProjects.clear(); // ### could keep the ones that persist somehow
    List<Path> projects = mOptions.dataDir.files(Path::File);
    const Path home = Path::home();
    for (int i=0; i<projects.size(); ++i) {
        Path file = projects.at(i);
        Path p = file.mid(mOptions.dataDir.size());
        RTags::decodePath(p);
        if (p.isDir()) {
            bool remove = false;
            if (FILE *f = fopen(file.constData(), "r")) {
                Deserializer in(f);
                int version;
                in >> version;

                if (version == Server::DatabaseVersion) {
                    int fs;
                    in >> fs;
                    if (fs != RTags::fileSize(f)) {
                        error("%s seems to be corrupted, refusing to restore. Removing.",
                              file.constData());
                        remove = true;
                    } else {
                        addProject(p);
                    }
                } else {
                    remove = true;
                    error() << file << "has wrong format. Got" << version << "expected" << Server::DatabaseVersion << "Removing";
                }
                fclose(f);
            }
            if (remove) {
                Path::rm(file);
            }
        }
    }
    return mProjects.size();
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
    const String raw = m->raw();
    if (!raw.isEmpty()) {
        if (!isCompletionStream(connection) && message->messageId() != CompileMessage::MessageId) {
            error() << raw;
        } else {
            warning() << raw;
        }
    }

    switch (message->messageId()) {
    case CompileMessage::MessageId:
        handleCompileMessage(static_cast<CompileMessage*>(message), connection);
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

void Server::handleCompileMessage(CompileMessage *message, Connection *conn)
{
    conn->finish(); // nothing to wait for
    shared_ptr<CompileJob> job(new CompileJob(*message));
    job->argsReady().connect(this, &Server::processSourceFile);
    mQueryThreadPool.start(job);
}

void Server::handleCreateOutputMessage(CreateOutputMessage *message, Connection *conn)
{
    LogObject *obj = new LogObject(conn, message->level());
    if (message->level() == CompilationError) {
        shared_ptr<Project> project = currentProject();
        if (project && project->isValid()) {
            const String errors = project->diagnostics();
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
    case QueryMessage::Builds:
        builds(*message, conn);
        break;
    case QueryMessage::IsIndexing:
        isIndexing(*message, conn);
        break;
    case QueryMessage::RemoveFile:
        removeFile(*message, conn);
        break;
    case QueryMessage::JSON:
        JSON(*message, conn);
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
    shared_ptr<Project> project = updateProjectForLocation(loc);
    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    FollowLocationJob job(loc, query, project);
    job.run(conn);
    conn->finish();
}

void Server::isIndexing(const QueryMessage &, Connection *conn)
{
    ProjectsMap copy;
    {
        MutexLocker lock(&mMutex);
        copy = mProjects;
    }
    for (ProjectsMap::const_iterator it = copy.begin(); it != copy.end(); ++it) {
        if (it->second->isIndexing()) {
            conn->write("1");
            conn->finish();
            return;
        }

    }
    conn->write("0");
    conn->finish();
}


void Server::removeFile(const QueryMessage &query, Connection *conn)
{
    // Path path = query.path();
    const Match match(query.query());
    shared_ptr<Project> project = updateProjectForLocation(match);
    if (!project) {
        project = currentProject();
        if (!project) {
            error("No project");
            conn->finish();
            return;
        }
    }

    const int count = project->remove(match);
    // error() << count << query.query();
    if (count) {
        conn->write<128>("Removed %d files", count);
    } else {
        conn->write("No matches");
    }
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

    shared_ptr<Project> project = updateProjectForLocation(loc);
    if (!project || !project->isValid()) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }
    const SourceInformation c = project->sourceInfo(fileId);
    if (c.isNull()) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }

    shared_ptr<IndexerJob> job(new IndexerJob(query, project, c));
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
    shared_ptr<Project> project = updateProjectForLocation(loc);

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
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (project && project->isValid()) {
        String out = project->fixIts(Location::fileId(path));
        if (!out.isEmpty())
            conn->write(out);
    }
    conn->finish();
}

void Server::JSON(const QueryMessage &query, Connection *conn)
{
    shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    }

    JSONJob job(query, project);
    job.run(conn);
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
    shared_ptr<Project> project = updateProjectForLocation(loc);

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
    const String name = query.query();

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
    const String partial = query.query();

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
    const String partial = query.query();

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

    shared_ptr<StatusJob> job(new StatusJob(query, project));
    job->setId(nextId());
    mPendingLookups[job->id()] = conn;
    startQueryJob(job);
}

void Server::isIndexed(const QueryMessage &query, Connection *conn)
{
    bool ok = false;
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (project) {
        if (path.isFile()) {
            ok = project->isIndexed(Location::fileId(path));
        } else if (path.isDir()) {
            ok = project->fileManager->contains(path);
        }
    }

    conn->write(ok ? "1" : "0");
    conn->finish();
}

void Server::hasFileManager(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (project && project->fileManager && (project->fileManager->contains(path) || project->match(path))) {
        conn->write("1");
    } else {
        conn->write("0");
    }
    conn->finish();
}

void Server::preprocessFile(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project || !project->isValid()) {
        conn->write("No project");
        conn->finish();
        return;
    }

    const uint32_t fileId = Location::fileId(path);
    const SourceInformation c = project->sourceInfo(fileId);
    if (c.isNull()) {
        conn->write("No arguments for " + path);
        conn->finish();
        return;
    }
    if (c.builds.size() <= query.buildIndex()) {
        conn->write<512>("No build for for index %d (max %d) for %s",
                         query.buildIndex(), c.builds.size() - 1, path.constData());
        conn->finish();
        return;
    }
        
    Preprocessor* pre = new Preprocessor(c, query.buildIndex(), conn);
    pre->preprocess();
}

void Server::clearProjects()
{
    MutexLocker lock(&mMutex);
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it)
        it->second->unload();
    RTags::removeDirectory(mOptions.dataDir);
    mCurrentProject.reset();
}

void Server::reindex(const QueryMessage &query, Connection *conn)
{
    Match match = query.match();
    shared_ptr<Project> project = updateProjectForLocation(match);
    if (!project) {
        project = currentProject();
        if (!project || !project->isValid()) {
            error("No project");
            conn->finish();
            return;
        }
    }

    const int count = project->reindex(match);
    // error() << count << query.query();
    if (count) {
        conn->write<128>("Dirtied %d files", count);
    } else {
        conn->write("No matches");
    }
    conn->finish();
}

void Server::startIndexerJob(const shared_ptr<IndexerJob> &job)
{
    mIndexerThreadPool->start(job);
}

void Server::startQueryJob(const shared_ptr<Job> &job)
{
    mQueryThreadPool.start(job);
}

void Server::processSourceFile(GccArguments args)
{
    if (args.lang() == GccArguments::NoLang)
        return;
    const Path srcRoot = args.projectRoot();
    List<Path> inputFiles = args.inputFiles();
    if (srcRoot.isEmpty()) {
        warning("Can't find project root for %s", String::join(inputFiles, ", ").constData());
        return;
    }

    debug() << inputFiles << "in" << srcRoot;
    const int count = inputFiles.size();
    int filtered = 0;
    if (!mOptions.excludeFilters.isEmpty()) {
        for (int i=0; i<count; ++i) {
            Path &p = inputFiles[i];
            if (Filter::filter(p, mOptions.excludeFilters) == Filter::Filtered) {
                error() << "Filtered out" << p;
                p.clear();
                ++filtered;
            }
        }
    }
    if (filtered == count) {
        warning("no input file?");
        return;
    }

    {
        MutexLocker lock(&mMutex);

        shared_ptr<Project> project = mProjects.value(srcRoot);
        if (!project) {
            project = addProject(srcRoot);
            assert(project);
        }
        loadProject(project);

        if (!mCurrentProject.lock()) {
            mCurrentProject = project;
        }

        const List<String> arguments = args.clangArgs();

        for (int i=0; i<count; ++i) {
            SourceInformation sourceInformation = project->sourceInfo(Location::insertFile(inputFiles.at(i)));
            bool index = false;
            if (sourceInformation.isNull()) {
                sourceInformation.sourceFile = inputFiles.at(i);
                sourceInformation.builds.append(SourceInformation::Build(args.compiler(), arguments));
                index = true;
            } else {
                List<SourceInformation::Build> &builds = sourceInformation.builds;
                for (int i=0; i<builds.size(); ++i) {
                    if (builds.at(i).compiler == args.compiler()) {
                        if (builds.at(i).args == arguments) {
                            break;
                        } else if (mOptions.options & AllowMultipleBuildsForSameCompiler) {
                            builds[i].args = arguments;
                            index = true;
                            break;
                        }
                    }
                }
            }
            if (index) {
                project->index(sourceInformation, IndexerJob::Makefile);
            } else {
                debug() << inputFiles.at(i) << " is not dirty. ignoring";
            }
        }
    }
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
    default:
        EventReceiver::event(event);
        break;
    }
}

void Server::loadProject(shared_ptr<Project> &project)
{
    assert(project);
    if (!project->isValid()) {
        assert(!project->isValid());
        project->init();

        if (mRestoreProjects)
            project->restore();
    }
}

shared_ptr<Project> Server::setCurrentProject(const Path &path) // lock always held
{
    ProjectsMap::iterator it = mProjects.find(path);
    if (it != mProjects.end()) {
        mCurrentProject = it->second;
        assert(mCurrentProject.lock());
        if (!it->second->isValid())
            loadProject(it->second);
        return it->second;
    }
    return shared_ptr<Project>();
}

shared_ptr<Project> Server::updateProjectForLocation(const Location &location)
{
    return updateProjectForLocation(location.path());
}

shared_ptr<Project> Server::updateProjectForLocation(const Path &path)
{
    return updateProjectForLocation(Match(path));
}

shared_ptr<Project> Server::updateProjectForLocation(const Match &match)
{
    shared_ptr<Project> cur = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    if (cur && cur->match(match))
        return cur;

    MutexLocker lock(&mMutex);
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->match(match)) {
            return setCurrentProject(it->second->path());
        }
    }
    return shared_ptr<Project>();
}

void Server::removeProject(const QueryMessage &query, Connection *conn)
{
    MutexLocker lock(&mMutex);
    const bool unload = query.type() == QueryMessage::UnloadProject;

    const Match match = query.match();
    ProjectsMap::iterator it = mProjects.begin();
    while (it != mProjects.end()) {
        ProjectsMap::iterator cur = it++;
        if (cur->second->match(match)) {
            if (mCurrentProject.lock() == it->second)
                mCurrentProject.reset();
            cur->second->unload();
            Path path = cur->first;
            conn->write<128>("%s project: %s", unload ? "Unloaded" : "Deleted", path.constData());
            if (!unload) {
                RTags::encodePath(path);
                Path::rm(mOptions.dataDir + path);
                mProjects.erase(cur);
            }
        }
    }
    conn->finish();
}

void Server::reloadProjects(const QueryMessage &query, Connection *conn)
{
    int old;
    {
        MutexLocker lock(&mMutex);
        old = mProjects.size();
    }
    const int cur = reloadProjects();
    conn->write<128>("Changed from %d to %d projects", old, cur);
    conn->finish();
}

bool Server::selectProject(const Match &match, Connection *conn)
{
    MutexLocker lock(&mMutex);
    Path selected;
    bool error = false;
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->match(match)) {
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
        shared_ptr<Project> current = mCurrentProject.lock();
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

bool Server::updateProject(const List<String> &projects)
{
    for (int i=0; i<projects.size(); ++i) {
        if (selectProject(projects.at(i), 0))
            return true;
    }
    return false;
}

void Server::project(const QueryMessage &query, Connection *conn)
{
    MutexLocker lock(&mMutex);
    if (query.query().isEmpty()) {
        const shared_ptr<Project> current = mCurrentProject.lock();
        for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            conn->write<128>("%s%s%s",
                             it->first.constData(),
                             it->second->isValid() ? " (loaded)" : "",
                             it->second == current ? " <=" : "");
        }
    } else {
        Path selected;
        bool error = false;
        const Match match = query.match();
        const ProjectsMap::const_iterator it = mProjects.find(match.pattern());
        bool ok = false;
        unsigned long long index = query.query().toULongLong(&ok);
        if (it != mProjects.end()) {
            selected = it->first;
        } else {
            for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
                assert(it->second);
                if (ok) {
                    if (!index) {
                        selected = it->first;
                    } else {
                        --index;
                    }
                }
                if (it->second->match(match)) {
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
            shared_ptr<Project> current = mCurrentProject.lock();
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
    MutexLocker lock(&mMutex);
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

void Server::builds(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (project) {
        const uint32_t fileId = Location::fileId(path);
        if (fileId) {
            const SourceInformation info = project->sourceInfo(fileId);
            conn->write(info.toString());
        }
    }
    conn->finish();
}

void Server::handleCompletionMessage(CompletionMessage *message, Connection *conn)
{
    updateProject(message->projects());
    const Path path = message->path();
    shared_ptr<Project> project = updateProjectForLocation(path);

    if (!project || !project->isValid()) {
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

void Server::startCompletion(const Path &path, int line, int column, int pos, const String &contents, Connection *conn)
{
    // error() << "starting completion" << path << line << column;
    if (!mOptions.completionCacheSize) {
        conn->finish();
        return;
    }
    shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project || !project->isValid()) {
        if (!isCompletionStream(conn))
            conn->finish();
        return;
    }
    const uint32_t fileId = Location::fileId(path);
    if (!fileId)
        return;

    CXIndex index;
    CXTranslationUnit unit;
    List<String> args;
    if (!project->fetchFromCache(path, args, index, unit)) {
        const SourceInformation info = project->sourceInfo(fileId);
        if (!info.isNull()) {
            project->reindex(path);
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

void Server::restoreFileIds()
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
bool Server::saveFileIds() const
{
    if (!Path::mkdir(mOptions.dataDir)) {
        error("Can't create directory [%s]", mOptions.dataDir.constData());
        return false;
    }
    const Path p = mOptions.dataDir + "fileids";
    FILE *f = fopen(p.constData(), "w");
    if (!f) {
        error("Can't open file %s", p.constData());
        return false;
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
    return true;
}
