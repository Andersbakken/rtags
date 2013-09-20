/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "Server.h"

#include "CompileMessage.h"
#include "CompletionJob.h"
#include "CreateOutputMessage.h"
#include "CursorInfoJob.h"
#include "DependenciesJob.h"
#include "Filter.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "IndexerJob.h"
#include "JSONJob.h"
#include "GccArguments.h"
#if defined(HAVE_V8) || defined(HAVE_YAJL)
#  include "JSONParser.h"
#endif
#include "ListSymbolsJob.h"
#include "LogObject.h"
#include "Match.h"
#include "Preprocessor.h"
#include "Project.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include "IndexerMessage.h"
#include "RTags.h"
#include "ReferencesJob.h"
#include "StatusJob.h"
#include <clang-c/Index.h>
#include <rct/Connection.h>
#include <rct/EventLoop.h>
#include <rct/SocketClient.h>
#include <rct/Log.h>
#include <rct/Message.h>
#include <rct/Messages.h>
#include <rct/Path.h>
#include <rct/Process.h>
#include <rct/Rct.h>
#include <rct/RegExp.h>
#include <stdio.h>

Server *Server::sInstance = 0;
Server::Server()
    : mVerbose(false), mJobId(0), mThreadPool(2), mCurrentFileId(0)
{
    assert(!sInstance);
    sInstance = this;

    mUnloadTimer.timeout().connect(std::bind(&Server::onUnload, this));
    mClearCompletionCacheTimer.timeout().connect(std::bind(&Server::clearCompletionCache, this));
    mIndex = clang_createIndex(0, 1);
}

Server::~Server()
{
    clear();
    assert(sInstance == this);
    sInstance = 0;
    Messages::cleanup();
    clang_disposeIndex(mIndex);
}

void Server::clear()
{
    Path::rm(mOptions.socketFile);
    mServer.reset();
    mProjects.clear();
}

bool Server::init(const Options &options)
{
    {
        Path dir(RTAGS_BIN);
        dir.resolve();
        List<Path> plugins = dir.files(Path::File);
        Path executableDir = Rct::executablePath().resolved().parentDir();
        if (dir != executableDir)
            plugins += executableDir.files(Path::File);
        for (int i=0; i<plugins.size(); ++i) {
            if (mPluginFactory.addPlugin(plugins.at(i))) {
                error() << "Loaded plugin" << plugins.at(i);
            }
        }
    }
    RTags::initMessages();

    mOptions = options;
    if (options.options & NoBuiltinIncludes) {
        mOptions.defaultArguments.append("-nobuiltininc");
        mOptions.defaultArguments.append("-nostdinc++");
    } else {
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
    error() << "using args:" << String::join(mOptions.defaultArguments, " ");

    if (mOptions.options & ClearProjects) {
        clearProjects();
    }

    for (int i=0; i<10; ++i) {
        mServer.reset(new SocketServer);
        if (mServer->listen(mOptions.socketFile)) {
            break;
        }
        mServer.reset();
        if (!i) {
            enum { Timeout = 1000 };
            Connection connection;
            if (connection.connectToServer(mOptions.socketFile, Timeout)) {
                connection.send(QueryMessage(QueryMessage::Shutdown));
                connection.disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                connection.finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                EventLoop::eventLoop()->exec(Timeout);
            }
        }
        sleep(1);
        Path::rm(mOptions.socketFile);
    }
    if (!mServer) {
        error("Unable to listen on %s", mOptions.socketFile.constData());
        return false;
    }

    restoreFileIds();
    mServer->newConnection().connect(std::bind(&Server::onNewConnection, this));
    reloadProjects();
    if (!(mOptions.options & NoStartupCurrentProject)) {
        Path current = Path(mOptions.dataDir + ".currentProject").readAll(1024);
        if (current.size() > 1) {
            current.chop(1);
            if (!setCurrentProject(current)) {
                error() << "Can't restore project" << current;
                unlink((mOptions.dataDir + ".currentProject").constData());
            }
        }
    }

    if (mOptions.clearCompletionCacheInterval && mOptions.completionCacheSize) {
        clearCompletionCache();
    }

    return true;
}

std::shared_ptr<Project> Server::addProject(const Path &path) // lock always held
{
    std::shared_ptr<Project> &project = mProjects[path];
    if (!project) {
        project.reset(new Project(path));
        return project;
    }
    return std::shared_ptr<Project>();
}

int Server::reloadProjects()
{
    std::lock_guard<std::mutex> lock(mMutex);
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
                    if (fs != Rct::fileSize(f)) {
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
        SocketClient::SharedPtr client = mServer->nextConnection();
        if (!client)
            break;
        Connection *conn = new Connection(client);
        conn->newMessage().connect(std::bind(&Server::onNewMessage, this, std::placeholders::_1, std::placeholders::_2));
        conn->disconnected().connect(std::bind(&Server::onConnectionDisconnected, this, std::placeholders::_1));
    }
}

void Server::onConnectionDisconnected(Connection *o)
{
    Map<int, Connection*>::iterator it = mPendingLookups.begin();
    const Map<int, Connection*>::const_iterator end = mPendingLookups.end();
    while (it != end) {
        if (it->second == o) {
            mPendingLookups.erase(it);
            break;
        } else {
            ++it;
        }
    }
    o->disconnected().disconnect();
    EventLoop::deleteLater(o);
}

void Server::onNewMessage(Message *message, Connection *connection)
{
    if (mOptions.unloadTimer)
        mUnloadTimer.restart(mOptions.unloadTimer * 1000 * 60, Timer::SingleShot);

    if (mOptions.clearCompletionCacheInterval && mOptions.completionCacheSize && !mClearCompletionCacheTimer.isRunning())
        mClearCompletionCacheTimer.restart(mOptions.clearCompletionCacheInterval * 1000 * 60, Timer::SingleShot);

    ClientMessage *m = static_cast<ClientMessage*>(message);
    const String raw = m->raw();
    if (!raw.isEmpty()) {
        if (!isCompletionStream(connection) && message->messageId() != CompileMessage::MessageId) {
            error() << raw;
        } else {
            warning() << raw;
        }
    }

    // error() << "Got a message" << static_cast<int>(message->messageId());

    switch (message->messageId()) {
    case CompileMessage::MessageId:
        handleCompileMessage(static_cast<const CompileMessage&>(*message), connection);
        break;
    case QueryMessage::MessageId:
        handleQueryMessage(static_cast<const QueryMessage&>(*message), connection);
        break;
    case IndexerMessage::MessageId:
        handleIndexerMessage(static_cast<const IndexerMessage&>(*message), connection);
        break;
    case CreateOutputMessage::MessageId:
        handleCreateOutputMessage(static_cast<const CreateOutputMessage&>(*message), connection);
        break;
    case CompletionMessage::MessageId: {
        const CompletionMessage &completionMessage = static_cast<const CompletionMessage&>(*message);
        if (completionMessage.flags() & CompletionMessage::Stream) {
            handleCompletionStream(completionMessage, connection);
        } else {
            handleCompletionMessage(completionMessage, connection);
        }
        break; }
    case ResponseMessage::MessageId:
    case FinishMessage::MessageId:
    case VisitFileMessage::MessageId:
        error() << "Unexpected message" << static_cast<int>(message->messageId());
        // assert(0);
        connection->finish();
        break;
    default:
        error("Unknown message: %d", message->messageId());
        connection->finish();
        break;
    }
    if (mOptions.options & NoFileManagerWatch) {
        std::shared_ptr<Project> project = currentProject();
        if (project && project->fileManager && (Rct::monoMs() - project->fileManager->lastReloadTime()) > 60000)
            project->fileManager->reload(FileManager::Asynchronous);
    }
}

void Server::handleCompileMessage(const CompileMessage &message, Connection *conn)
{
    conn->finish(); // nothing to wait for
    const Path workingDirectory = message.workingDirectory();
    const String arguments = message.arguments();
    const List<String> projects = message.projects();
    assert(message.workingDirectory().endsWith('/'));
    if (arguments.endsWith(".js") && !arguments.contains(' ')) {
        if (mOptions.options & NoEsprima)
            return;
        Path jsFile = arguments;
        if (!jsFile.isAbsolute())
            jsFile.prepend(workingDirectory);
        const Path srcRoot = RTags::findProjectRoot(jsFile);
        if (srcRoot.isEmpty()) {
            error() << "Can't find project root for" << jsFile;
            return;
        }
        {
            std::lock_guard<std::mutex> lock(mMutex);

            std::shared_ptr<Project> project = mProjects.value(srcRoot);
            if (!project) {
                project = addProject(srcRoot);
                assert(project);
            }
            project->load();

            if (!mCurrentProject.lock())
                mCurrentProject = project;

            project->index(jsFile.resolved());
        }
    } else {
        GccArguments args;
        if (args.parse(arguments, workingDirectory))
            index(args, projects);
    }
}

void Server::handleCreateOutputMessage(const CreateOutputMessage &message, Connection *conn)
{
    new LogObject(conn, message.level());
}

void Server::handleIndexerMessage(const IndexerMessage &message, Connection *conn)
{
    std::shared_ptr<IndexData> indexData = message.data();
    assert(indexData);
    std::shared_ptr<Project> project = mProjects.value(message.project());
    if (!project) {
        error() << "Can't find project root for this IndexerMessage" << message.project() << Location::path(indexData->fileId);
        return;
    }
    project->onJobFinished(indexData);
}

void Server::handleQueryMessage(const QueryMessage &message, Connection *conn)
{
    conn->setSilent(message.flags() & QueryMessage::Silent);
    updateProject(message.projects(), message.flags());

    switch (message.type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::Builds:
        builds(message, conn);
        break;
    case QueryMessage::SuspendFile:
        suspendFile(message, conn);
        break;
    case QueryMessage::IsIndexing:
        isIndexing(message, conn);
        break;
    case QueryMessage::RemoveFile:
        removeFile(message, conn);
        break;
    case QueryMessage::CodeCompletionEnabled:
        codeCompletionEnabled(message, conn);
        break;
    case QueryMessage::JSON:
        JSON(message, conn);
        break;
    case QueryMessage::JobCount:
        jobCount(message, conn);
        break;
    case QueryMessage::FixIts:
        fixIts(message, conn);
        break;
    case QueryMessage::FindFile:
        findFile(message, conn);
        break;
    case QueryMessage::DumpFile:
        dumpFile(message, conn);
        break;
    case QueryMessage::Dependencies:
        dependencies(message, conn);
        break;
    case QueryMessage::DeleteProject:
        removeProject(message, conn);
        break;
    case QueryMessage::UnloadProject:
        removeProject(message, conn);
        break;
    case QueryMessage::ReloadProjects:
        reloadProjects(message, conn);
        break;
    case QueryMessage::Project:
        project(message, conn);
        break;
    case QueryMessage::LoadCompilationDatabase:
        loadCompilationDatabase(message, conn);
        break;
    case QueryMessage::Reindex: {
        reindex(message, conn);
        break; }
    case QueryMessage::ClearProjects:
        clearProjects(message, conn);
        break;
    case QueryMessage::CursorInfo:
        cursorInfo(message, conn);
        break;
    case QueryMessage::Shutdown:
        shutdown(message, conn);
        break;
    case QueryMessage::FollowLocation:
        followLocation(message, conn);
        break;
    case QueryMessage::ReferencesLocation:
        referencesForLocation(message, conn);
        break;
    case QueryMessage::ReferencesName:
        referencesForName(message, conn);
        break;
    case QueryMessage::ListSymbols:
        listSymbols(message, conn);
        break;
    case QueryMessage::FindSymbols:
        findSymbols(message, conn);
        break;
    case QueryMessage::Status:
        status(message, conn);
        break;
    case QueryMessage::IsIndexed:
        isIndexed(message, conn);
        break;
    case QueryMessage::HasFileManager:
        hasFileManager(message, conn);
        break;
    case QueryMessage::PreprocessFile:
        preprocessFile(message, conn);
        break;
    case QueryMessage::ReloadFileManager:
        reloadFileManager(message, conn);
        break;
    case QueryMessage::VisitFile:
        visitFile(message, conn);
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
    std::shared_ptr<Project> project = updateProjectForLocation(loc.path());
    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
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
        std::lock_guard<std::mutex> lock(mMutex);
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
    const Match match = query.match();
    std::shared_ptr<Project> project = updateProjectForLocation(match);
    if (!project)
        project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
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
    std::shared_ptr<Project> project = currentProject();
    if (!project || project->state() == Project::Unloaded) {
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

    std::shared_ptr<Project> project = updateProjectForLocation(loc.path());
    if (!project || project->state() != Project::Loaded) {
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

#warning needs fixin
 /*    std::shared_ptr<IndexerJob> job = Server::instance()->factory().createJob(query, project, c); */
/*     if (job) { */
/*         job->setId(nextId()); */
/*         mPendingLookups[job->id()] = conn; */
/*         mThreadPool.start(job); */
/*     } else { */
/*         conn->write<128>("Failed to create job for %s", c.sourceFile().constData()); */
/*         conn->finish(); */
/*     } */
}

void Server::cursorInfo(const QueryMessage &query, Connection *conn)
{
    const Location loc = query.location();
    if (loc.isNull()) {
        conn->finish();
        return;
    }
    std::shared_ptr<Project> project = updateProjectForLocation(loc.path());

    if (!project) {
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }


    CursorInfoJob job(loc, query, project);
    job.run(conn);
    conn->finish();
}

void Server::codeCompletionEnabled(const QueryMessage &, Connection *conn)
{
    conn->write(mOptions.completionCacheSize ? "1" : "0");
    conn->finish();
}

void Server::dependencies(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    std::shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project) {
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    DependenciesJob job(query, project);
    job.run(conn);
    conn->finish();
}

void Server::fixIts(const QueryMessage &query, Connection *conn)
{
    std::shared_ptr<Project> project = updateProjectForLocation(query.match());
    if (project && project->state() == Project::Loaded) {
        String out = project->fixIts(Location::fileId(query.query()));
        if (!out.isEmpty())
            conn->write(out);
    }
    conn->finish();
}

void Server::JSON(const QueryMessage &query, Connection *conn)
{
    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        conn->write("No project");
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
    } else {
        JSONJob job(query, project);
        job.run(conn);
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
    std::shared_ptr<Project> project = updateProjectForLocation(loc.path());

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
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

    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
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

    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
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

    std::shared_ptr<Project> project = currentProject();
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
    std::shared_ptr<Project> project = currentProject();

    if (!project) {
        error("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
        return;
    }

    conn->client()->setWriteMode(SocketClient::Synchronous);

    StatusJob job(query, project);
    job.run(conn);
    conn->finish();
}

void Server::isIndexed(const QueryMessage &query, Connection *conn)
{
    int ret = 0;
    const Match match = query.match();
    std::shared_ptr<Project> project = updateProjectForLocation(match);
    if (project) {
        bool indexed = false;
        if (project->match(match, &indexed))
            ret = indexed ? 1 : 2;
    }

    error("=> %d", ret);
    conn->write<16>("%d", ret);
    conn->finish();
}

void Server::reloadFileManager(const QueryMessage &, Connection *conn)
{
    std::shared_ptr<Project> project = currentProject();
    if (project) {
        conn->write<512>("Reloading files for %s", project->path().constData());
        conn->finish();
        project->fileManager->reload(FileManager::Asynchronous);
    } else {
        conn->write("No current project");
        conn->finish();
    }
}


void Server::hasFileManager(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    std::shared_ptr<Project> project = updateProjectForLocation(path);
    if (project && project->fileManager && (project->fileManager->contains(path) || project->match(query.match()))) {
        error("=> 1");
        conn->write("1");
    } else {
        error("=> 0");
        conn->write("0");
    }
    conn->finish();
}

void Server::preprocessFile(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    std::shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project) {
        conn->write("No project");
        conn->finish();
        return;
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
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

    Preprocessor* pre = new Preprocessor(c, conn);
    pre->preprocess();
}

void Server::clearProjects()
{
    std::lock_guard<std::mutex> lock(mMutex);
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it)
        it->second->unload();
    Rct::removeDirectory(mOptions.dataDir);
    mCurrentProject.reset();
    unlink((mOptions.dataDir + ".currentProject").constData());
    mProjects.clear();
}

void Server::reindex(const QueryMessage &query, Connection *conn)
{
    Match match = query.match();
    std::shared_ptr<Project> project = updateProjectForLocation(match);
    if (!project) {
        project = currentProject();
        if (!project) {
            error("No project");
            conn->finish();
            return;
        } else if (project->state() != Project::Loaded) {
            conn->write("Project loading");
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

void Server::index(const GccArguments &args, const List<String> &projects)
{
#warning need to resolve args.includes() and stick them into SourceInformation::additionalDependencies
    if (args.lang() == GccArguments::NoLang || mOptions.ignoredCompilers.contains(args.compiler())) {
        return;
    }
    Path srcRoot;
    if (updateProject(projects, 0)) {
        srcRoot = currentProject()->path();
    } else if (!projects.isEmpty()) {
        srcRoot = projects.first();
    } else {
        srcRoot = args.projectRoot();
    }
    if (srcRoot.isEmpty()) {
        error("Can't find project root for %s", String::join(args.inputFiles(), ", ").constData());
        return;
    }

    List<Path> inputFiles = args.inputFiles();
    debug() << inputFiles << "in" << srcRoot;
    int count = inputFiles.size();
    if (!mOptions.excludeFilters.isEmpty()) {
        int i = 0;
        while (i < count) {
            if (Filter::filter(inputFiles.at(i), mOptions.excludeFilters) == Filter::Filtered) {
                debug() << "Filtered out" << inputFiles.at(i);
                inputFiles.removeAt(i);
                --count;
            } else {
                ++i;
            }
        }
    }
    if (!count) {
        warning("no input files?");
        return;
    }

    {
        std::lock_guard<std::mutex> lock(mMutex);

        std::shared_ptr<Project> project = mProjects.value(srcRoot);
        if (!project) {
            project = addProject(srcRoot);
            assert(project);
        }
        project->load();

        if (!mCurrentProject.lock()) {
            mCurrentProject = project;
            setupCurrentProjectFile(project);
        }

        const List<String> arguments = args.clangArgs();

        for (int i=0; i<count; ++i) {
            project->index(inputFiles.at(i), args.compiler(), arguments);
        }
    }
}

void Server::onJobOutput(JobOutput&& out)
{
    Map<int, Connection*>::iterator it = mPendingLookups.find(out.id);
    if (it == mPendingLookups.end()) {
        error() << "Can't find connection for id" << out.id;
        if (std::shared_ptr<Job> job = out.job.lock())
            job->abort();
        return;
    }
    Connection* conn = it->second;
    if (!conn->isConnected()) {
        error() << "Connection has been disconnected";
        if (std::shared_ptr<Job> job = out.job.lock())
            job->abort();
        return;
    }
    if (!out.out.isEmpty() && !conn->write(out.out)) {
        error() << "Failed to write to connection";
        if (std::shared_ptr<Job> job = out.job.lock())
            job->abort();
        return;
    }

    if (out.finish) {
        if (isCompletionStream(conn))
            mPendingLookups.erase(it);
        else
            it->second->finish();
    }
}

std::shared_ptr<Project> Server::setCurrentProject(const Path &path, unsigned int queryFlags) // lock always held
{
    ProjectsMap::iterator it = mProjects.find(path);
    if (it != mProjects.end()) {
        setCurrentProject(it->second, queryFlags);
        return it->second;
    }
    return std::shared_ptr<Project>();
}

void Server::setupCurrentProjectFile(const std::shared_ptr<Project> &project)
{
    if (project) {
        Path::mkdir(mOptions.dataDir);
        FILE *f = fopen((mOptions.dataDir + ".currentProject").constData(), "w");
        if (f) {
            if (!fwrite(project->path().constData(), project->path().size(), 1, f) || !fwrite("\n", 1, 1, f)) {
                error() << "error writing to" << (mOptions.dataDir + ".currentProject");
                fclose(f);
                unlink((mOptions.dataDir + ".currentProject").constData());
            } else {
                fclose(f);
            }
        } else {
            error() << "error opening" << (mOptions.dataDir + ".currentProject") << "for write";
        }
    } else {
        Path::rm(mOptions.dataDir + ".currentProject");
    }
}

std::shared_ptr<Project> Server::setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags)
{
    std::shared_ptr<Project> old = mCurrentProject.lock();
    if (project && project != old) {
        if (old && old->fileManager)
            old->fileManager->clearFileSystemWatcher();
        mCurrentProject = project;
        setupCurrentProjectFile(project);

        Project::FileManagerMode mode = Project::FileManager_Asynchronous;
        if (queryFlags & QueryMessage::WaitForLoadProject)
            mode = Project::FileManager_Synchronous;
        switch (project->state()) {
        case Project::Loaded:
        case Project::Inited:
            project->fileManager->reload(FileManager::Asynchronous);
            break;
        default:
            break;
        }
        project->load(mode);
        return project;
    }
    return std::shared_ptr<Project>();
}

std::shared_ptr<Project> Server::updateProjectForLocation(const Match &match)
{
    std::shared_ptr<Project> cur = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    if (cur && cur->match(match))
        return cur;

    std::lock_guard<std::mutex> lock(mMutex);
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->match(match)) {
            return setCurrentProject(it->second->path());
        }
    }
    return std::shared_ptr<Project>();
}

void Server::removeProject(const QueryMessage &query, Connection *conn)
{
    std::lock_guard<std::mutex> lock(mMutex);
    const bool unload = query.type() == QueryMessage::UnloadProject;

    const Match match = query.match();
    ProjectsMap::iterator it = mProjects.begin();
    while (it != mProjects.end()) {
        ProjectsMap::iterator cur = it++;
        if (cur->second->match(match)) {
            if (mCurrentProject.lock() == cur->second) {
                mCurrentProject.reset();
                setupCurrentProjectFile(std::shared_ptr<Project>());
                unlink((mOptions.dataDir + ".currentProject").constData());
            }
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
        std::lock_guard<std::mutex> lock(mMutex);
        old = mProjects.size();
    }
    const int cur = reloadProjects();
    conn->write<128>("Changed from %d to %d projects", old, cur);
    conn->finish();
}

bool Server::selectProject(const Match &match, Connection *conn, unsigned int queryFlags)
{
    std::lock_guard<std::mutex> lock(mMutex);
    std::shared_ptr<Project> selected;
    bool error = false;
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->match(match)) {
            if (error) {
                if (conn)
                    conn->write(it->first);
            } else if (selected) {
                error = true;
                if (conn) {
                    conn->write<128>("Multiple matches for %s", match.pattern().constData());
                    conn->write(selected->path());
                    conn->write(it->first);
                }
                selected.reset();
            } else {
                selected = it->second;
                const Path p = match.pattern();
                if (p.isFile()) {
                    // ### this needs to deal with dependencies for headers
                    mCurrentFileId = Location::fileId(p);
                } else {
                    mCurrentFileId = 0;
                }
            }
        }
    }
    if (selected) {
        if (setCurrentProject(selected, queryFlags) && conn)
            conn->write<128>("Selected project: %s for %s", selected->path().constData(), match.pattern().constData());
        return true;
    } else if (!error && conn) {
        conn->write<128>("No matches for %s", match.pattern().constData());
    }
    return false;
}

bool Server::updateProject(const List<String> &projects, unsigned int queryFlags)
{
    for (int i=0; i<projects.size(); ++i) {
        if (selectProject(projects.at(i), 0, queryFlags))
            return true;
    }
    return false;
}

void Server::project(const QueryMessage &query, Connection *conn)
{
    std::lock_guard<std::mutex> lock(mMutex);
    if (query.query().isEmpty()) {
        const std::shared_ptr<Project> current = mCurrentProject.lock();
        const char *states[] = { "(unloaded)", "(inited)", "(loading)", "(loaded)" };
        for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            conn->write<128>("%s %s%s",
                             it->first.constData(),
                             states[it->second->state()],
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
            std::shared_ptr<Project> current = mCurrentProject.lock();
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
    std::lock_guard<std::mutex> lock(mMutex);
    if (query.query().isEmpty()) {
        conn->write<128>("Running with %d jobs", mOptions.processCount);
    } else {
        const int jobCount = query.query().toLongLong();
        if (jobCount <= 0 || jobCount > 100) {
            conn->write<128>("Invalid job count %s (%d)", query.query().constData(), jobCount);
        } else {
            mOptions.processCount = jobCount;
            mProcessPool.setCount(jobCount);
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

void Server::loadCompilationDatabase(const QueryMessage &query, Connection *conn)
{
#if defined(HAVE_V8) || defined(HAVE_YAJL)
    const Path path = query.query();
    const String json = path.readAll();
    JSONParser parser(json);
    if (!parser.isValid()) {
        conn->write("Can't parse compilation database");
        conn->finish();
        return;
    }

    const Value& root = parser.root();
    bool ok = true;
    if (root.type() == Value::Type_List) {
        const List<Value>& items = root.toList();
        for (int i = 0; i < items.size(); ++i) {
            const Value& item = items.at(i);
            if (item.type() != Value::Type_Map) {
                ok = false;
                break;
            }
            const Map<String, Value>& entry = item.toMap();
            Map<String, Value>::const_iterator entryItem = entry.begin();
            const Map<String, Value>::const_iterator entryEnd = entry.end();
            Path dir;
            String args;
            while (entryItem != entryEnd) {
                if (entryItem->first == "directory" && entryItem->second.type() == Value::Type_String)
                    dir = entryItem->second.toString();
                else if (entryItem->first == "command" && entryItem->second.type() == Value::Type_String)
                    args = entryItem->second.toString();
                ++entryItem;
            }
            if (!dir.isEmpty() && !args.isEmpty()) {
                //error() << "parsing" << args;
                args.replace("\\\"", "\"");
                GccArguments gccArgs;
                if (gccArgs.parse(args, dir)) {
                    index(gccArgs, query.projects());
                } else {
                    ok = false;
                    break;
                }
            } else {
                ok = false;
            }
        }
    } else {
        ok = false;
    }
    if (ok) {
        conn->write("Compilation database loaded");
    } else {
        conn->write("Invalid compilation database");
    }
#else
    conn->write("No JSON parser available, need either V8 or YAJL at compile-time");
#endif
    conn->finish();
}

void Server::shutdown(const QueryMessage &query, Connection *conn)
{
    for (Map<Path, std::shared_ptr<Project> >::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second)
            it->second->unload();
    }
    EventLoop::eventLoop()->quit();
    conn->write("Shutting down");
    conn->finish();
}

void Server::builds(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    if (!path.isEmpty()) {
        std::shared_ptr<Project> project = updateProjectForLocation(path);
        if (project) {
            if (project->state() != Project::Loaded) {
                conn->write("Project loading");
            } else {
                const uint32_t fileId = Location::fileId(path);
                if (fileId) {
                    const SourceInformation info = project->sourceInfo(fileId);
                    conn->write(info.toString());
                }
            }
        }
    } else if (std::shared_ptr<Project> project = currentProject()) {
        if (project->state() != Project::Loaded) {
            conn->write("Project loading");
        } else {
            const SourceInformationMap infos = project->sourceInfos();
            for (SourceInformationMap::const_iterator it = infos.begin(); it != infos.end(); ++it) {
                conn->write(it->second.toString());
            }
        }
    } else {
        conn->write("No project");
    }
    conn->finish();
}

void Server::suspendFile(const QueryMessage &query, Connection *conn)
{
    std::shared_ptr<Project> project;
    const Match match = query.match();
    if (match.isEmpty() || match.pattern() == "clear") {
        project = currentProject();
    } else {
        project = updateProjectForLocation(match);
    }
    if (!project) {
        conn->write("No project");
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
    } else {
        if (match.isEmpty()) {
            const Set<uint32_t> suspendedFiles = project->suspendedFiles();
            if (suspendedFiles.isEmpty()) {
                conn->write<512>("No files suspended for project %s", project->path().constData());
            } else {
                for (Set<uint32_t>::const_iterator it = suspendedFiles.begin(); it != suspendedFiles.end(); ++it)
                    conn->write<512>("%s is suspended", Location::path(*it).constData());
            }
        } else {
            const Path p = query.match().pattern();
            if (p == "clear") {
                project->clearSuspendedFiles();
                conn->write<512>("No files are suspended");
            } else if (!p.isFile()) {
                conn->write<512>("%s doesn't seem to exist", p.constData());
            } else {
                const uint32_t fileId = Location::insertFile(p);
                conn->write<512>("%s is no%s suspended", p.constData(),
                                 project->toggleSuspendFile(fileId) ? "w" : " longer");
            }
        }
    }
    conn->finish();
}

void Server::visitFile(const QueryMessage &query, Connection *conn)
{
    std::shared_ptr<Project> project = currentProject();
    // give current a chance first to avoid switching project when using system headers etc
    const Path path = query.query();
    if (!project || !project->match(path)) {
        for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
            if (it->second->match(path)) {
                project = it->second;
                break;
            }
        }
        if (!project) {
            conn->write("-1");
            conn->finish();
            return;
        }
    }

    const uint32_t fileId = Location::insertFile(path);
    const bool visit = project->visitFile(fileId);
    VisitFileMessage msg(fileId, visit);
    conn->send(msg);
    conn->finish();
}

void Server::handleCompletionMessage(const CompletionMessage &message, Connection *conn)
{
    updateProject(message.projects(), 0);
    const Path path = message.path();
    std::shared_ptr<Project> project = updateProjectForLocation(path);

    if (!project || project->state() != Project::Loaded) {
        if (!isCompletionStream(conn))
            conn->finish();
        return;
    }
    if (mActiveCompletions.contains(path)) {
        PendingCompletion &pending = mPendingCompletions[path];
        pending.line = message.line();
        pending.column = message.column();
        pending.pos = message.pos();
        pending.contents = message.contents();
        pending.connection = conn;
    } else {
        startCompletion(path, message.line(), message.column(), message.pos(), message.contents(), conn);
    }
}

void Server::startCompletion(const Path &path, int line, int column, int pos, const String &contents, Connection *conn)
{
#warning not done
    // {
    //     std::lock_guard<std::mutex> lock(mMutex);
    //     mCurrentFileId = Location::fileId(path);
    // }

    // // error() << "starting completion" << path << line << column;
    // if (!mOptions.completionCacheSize) {
    //     conn->finish();
    //     return;
    // }
    // std::shared_ptr<Project> project = updateProjectForLocation(path);
    // if (!project || project->state() != Project::Loaded) {
    //     if (!isCompletionStream(conn))
    //         conn->finish();
    //     return;
    // }
    // const uint32_t fileId = Location::fileId(path);
    // if (!fileId)
    //     return;

    // CXTranslationUnit unit;
    // List<String> args;
    // int parseCount = 0;
    // if (!project->fetchFromCache(path, args, unit, &parseCount)) {
    //     const SourceInformation info = project->sourceInfo(fileId);
    //     if (info.isNull() || info.isJS()) {
    //         if (!isCompletionStream(conn))
    //             conn->finish();
    //         return;
    //     }
    //     assert(!info.args.isEmpty());
    //     args = info.args;
    // }

    // mActiveCompletions.insert(path);
    // std::shared_ptr<CompletionJob> job(new CompletionJob(project, isCompletionStream(conn) ? CompletionJob::Stream : CompletionJob::Sync));
    // job->init(unit, path, args, line, column, pos, contents, parseCount);
    // job->setId(nextId());
    // job->finished().connect<EventLoop::Async>(std::bind(&Server::onCompletionJobFinished, this, std::placeholders::_1, std::placeholders::_2));
    // mPendingLookups[job->id()] = conn;
    // startQueryJob(job);
}

void Server::onCompletionJobFinished(Path path, int /*id*/)
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
    SocketClient::SharedPtr client = conn->client();
    return (mCompletionStreams.find(client) != mCompletionStreams.end());
}

void Server::onCompletionStreamDisconnected(const SocketClient::SharedPtr& client)
{
    mCompletionStreams.remove(client);
}

void Server::handleCompletionStream(const CompletionMessage &message, Connection *conn)
{
    SocketClient::SharedPtr client = conn->client();
    assert(client);
    client->disconnected().connect(std::bind(&Server::onCompletionStreamDisconnected, this, std::placeholders::_1));
    mCompletionStreams[client] = conn;
}

void Server::restoreFileIds()
{
    const Path p = mOptions.dataDir + "fileids";
    bool clear = true;
    FILE *f = fopen(p.constData(), "r");
    if (f) {
        Map<Path, uint32_t> pathsToIds;
        Deserializer in(f);
        int version;
        in >> version;
        if (version == DatabaseVersion) {
            int size;
            in >> size;
            if (size != Rct::fileSize(f)) {
                error("Refusing to load corrupted file %s", p.constData());
            } else {
                in >> pathsToIds;
                clear = false;
                Location::init(pathsToIds);
            }
            fclose(f);
        } else {
            error() << p << "has wrong format. Got" << version << "expected" << Server::DatabaseVersion << ", can't restore anything";
        }
    }
    if (clear)
        clearProjects();
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

void Server::onUnload()
{
    std::lock_guard<std::mutex> lock(mMutex);
    std::shared_ptr<Project> cur = mCurrentProject.lock();
    for (ProjectsMap::const_iterator it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->state() != Project::Unloaded && it->second != cur && !it->second->isIndexing()) {
            it->second->unload();
        }
    }
}

Path::VisitResult clearCompletionCacheCallback(const Path &path, void *rx)
{
    if (path.isFile() && reinterpret_cast<RegExp*>(rx)->indexIn(path) != -1) {
        path.rm();
    }

    return Path::Continue;
}

void Server::clearCompletionCache()
{
    Path path("/tmp/");
    RegExp rx("/preamble.pch-[A-Za-z0-9][A-Za-z0-9][A-Za-z0-9][A-Za-z0-9][A-Za-z0-9][A-Za-z0-9]$");
    path.visit(::clearCompletionCacheCallback, &rx);
}
