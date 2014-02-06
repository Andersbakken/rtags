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

#include "CompletionThread.h"
#include "CompileMessage.h"
#include "LogOutputMessage.h"
#include "CursorInfoJob.h"
#include "DependenciesJob.h"
#include "VisitFileResponseMessage.h"
#include "JobAnnouncementMessage.h"
#include "ProxyJobAnnouncementMessage.h"
#include "ClientMessage.h"
#include "PreprocessJob.h"
#include "Filter.h"
#include "FindFileJob.h"
#include "FindSymbolsJob.h"
#include "FollowLocationJob.h"
#include "IndexerJob.h"
#include "Source.h"
#include "Cpp.h"
#include "DumpThread.h"
#if defined(HAVE_CXCOMPILATIONDATABASE)
#  include <clang-c/CXCompilationDatabase.h>
#endif
#include "ListSymbolsJob.h"
#include "LogObject.h"
#include "Match.h"
#include "Preprocessor.h"
#include "Project.h"
#include "QueryMessage.h"
#include "VisitFileMessage.h"
#include "IndexerMessage.h"
#include "JobRequestMessage.h"
#include "JobResponseMessage.h"
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
#include <arpa/inet.h>
#include <limits>

class HttpLogObject : public LogOutput
{
public:
    HttpLogObject(int logLevel, const SocketClient::SharedPtr &socket)
        : LogOutput(logLevel), mSocket(socket)
    {}

    virtual bool testLog(int level) const
    {
        return level == logLevel();
    }
    virtual void log(const char *msg, int len)
    {
        if (!EventLoop::isMainThread()) {
            String message(msg, len);
            SocketClient::WeakPtr weak = mSocket;

            EventLoop::eventLoop()->callLater(std::bind([message,weak,this] {
                        // ### I don't understand why I need to capture this
                        // ### here (especially since this potentially could
                        // ### have been destroyed but it doesn't compile
                        // ### otherwise.
                        if (SocketClient::SharedPtr socket = weak.lock()) {
                            HttpLogObject::send(message.constData(), message.size(), socket);
                        }
                    }));
        } else {
            send(msg, len, mSocket);
        }
    }
    static void send(const char *msg, int len, const SocketClient::SharedPtr &socket)
    {
        static const unsigned char *header = reinterpret_cast<const unsigned char*>("data:");
        static const unsigned char *crlf = reinterpret_cast<const unsigned char*>("\r\n");
        socket->write(header, 5);
        socket->write(reinterpret_cast<const unsigned char *>(msg), len);
        socket->write(crlf, 2);
    }
private:
    SocketClient::SharedPtr mSocket;
};

static const bool debugMulti = getenv("RDM_DEBUG_MULTI");

Server *Server::sInstance = 0;
Server::Server()
    : mVerbose(false), mCurrentFileId(0), mThreadPool(0), mRemotePending(0),
      mServerConnection(0), mLastJobAnnouncementCount(0), mHostName(Rct::hostName()),
      mCompletionThread(0)
{
    Messages::registerMessage<JobRequestMessage>();
    Messages::registerMessage<JobResponseMessage>();

    assert(!sInstance);
    sInstance = this;

    mUnloadTimer.timeout().connect(std::bind(&Server::onUnload, this));
    mConnectToServerTimer.timeout().connect(std::bind(&Server::connectToServer, this));
    mRescheduleTimer.timeout().connect(std::bind(&Server::onReschedule, this));
}

Server::~Server()
{
    if (mCompletionThread) {
        mCompletionThread->stop();
        mCompletionThread->join();
        delete mCompletionThread;
        mCompletionThread = 0;
    }

    clear();
    assert(sInstance == this);
    sInstance = 0;
    Messages::cleanup();
}

void Server::clear()
{
    stopServers();
    delete mThreadPool; // wait first?
    mThreadPool = 0;
}

bool Server::init(const Options &options)
{
    RTags::initMessages();

    mOptions = options;
    mRescheduleTimer.restart(mOptions.rescheduleTimeout);
    Path clangPath = Path::resolved(CLANG_INCLUDEPATH);
    mOptions.includePaths.append(clangPath);
#ifdef OS_Darwin
    if (clangPath.exists()) {
        clangPath += "../../../c++/v1/";
        clangPath.resolve();
        if (clangPath.isDir())
            mOptions.includePaths.append(clangPath);
        // this seems to be the only way we get things like cstdint
    }
#endif

    if (options.options & UnlimitedErrors)
        mOptions.defaultArguments << "-ferror-limit=0";
    if (options.options & Wall)
        mOptions.defaultArguments << "-Wall";
    if (options.options & SpellChecking)
        mOptions.defaultArguments << "-fspell-checking";
    Log l(Error);
    l << "Running with" << mOptions.jobCount << "jobs, using args:"
      << String::join(mOptions.defaultArguments, ' ') << '\n';
    if (mOptions.tcpPort || mOptions.multicastPort || mOptions.httpPort) {
        if (mOptions.tcpPort)
            l << "tcp-port:" << mOptions.tcpPort;
        if (mOptions.multicastPort)
            l << "multicast-port:" << mOptions.multicastPort;
        if (mOptions.httpPort)
            l << "http-port:" << mOptions.httpPort;
        l << '\n';
    }
    l << "includepaths" << String::join(mOptions.includePaths, ' ');

    if (mOptions.options & ClearProjects) {
        clearProjects();
    }

    for (int i=0; i<10; ++i) {
        mUnixServer.reset(new SocketServer);
        if (mUnixServer->listen(mOptions.socketFile)) {
            break;
        }
        mUnixServer.reset();
        if (!i) {
            enum { Timeout = 1000 };
            Connection connection;
            if (connection.connectUnix(mOptions.socketFile, Timeout)) {
                connection.send(QueryMessage(QueryMessage::Shutdown));
                connection.disconnected().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                connection.finished().connect(std::bind([](){ EventLoop::eventLoop()->quit(); }));
                EventLoop::eventLoop()->exec(Timeout);
            }
        } else {
            sleep(1);
        }
        Path::rm(mOptions.socketFile);
    }
    if (!mUnixServer) {
        error("Unable to listen on %s", mOptions.socketFile.constData());
        return false;
    }

    restoreFileIds();
    mUnixServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
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

    if (!mOptions.multicastAddress.isEmpty()) {
        mMulticastSocket.reset(new SocketClient);
        if (!mMulticastSocket->bind(mOptions.multicastPort)) {
            error() << "Can't bind to multicast port" << mOptions.multicastPort;
        }
        if (!mMulticastSocket->addMembership(mOptions.multicastAddress)) {
            error() << "Can't add membership" << mOptions.multicastAddress;
            return false;
        }
        mMulticastSocket->setMulticastLoop(false);
        if (mOptions.multicastTTL)
            mMulticastSocket->setMulticastTTL(mOptions.multicastTTL);
        mMulticastSocket->readyReadFrom().connect(std::bind(&Server::onMulticastReadyRead, this,
                                                            std::placeholders::_1,
                                                            std::placeholders::_2,
                                                            std::placeholders::_3,
                                                            std::placeholders::_4));
    }

    if (mOptions.tcpPort) {
        mTcpServer.reset(new SocketServer);
        if (!mTcpServer->listen(mOptions.tcpPort)) {
            error() << "Unable to listen on port" << mOptions.tcpPort;
            return false;
        }

        mTcpServer->newConnection().connect(std::bind(&Server::onNewConnection, this, std::placeholders::_1));
    }
    if (!(mOptions.options & NoJobServer))
        mThreadPool = new ThreadPool(std::max(1, mOptions.jobCount));
    if (mOptions.httpPort) {
        mHttpServer.reset(new SocketServer);
        if (!mHttpServer->listen(mOptions.httpPort)) {
            error() << "Unable to listen on http-port:" << mOptions.httpPort;
            // return false;
            mHttpServer.reset();
        } else {
            mHttpServer->newConnection().connect(std::bind([this](){
                        while (SocketClient::SharedPtr client = mHttpServer->nextConnection()) {
                            mHttpClients[client] = 0;
                            client->disconnected().connect(std::bind([this, client] { mHttpClients.remove(client); }));
                            client->readyRead().connect(std::bind(&Server::onHttpClientReadyRead, this, std::placeholders::_1));
                        }
                    }));
        }

    }

    if (!(mOptions.options & JobServer))
        connectToServer();

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

void Server::onNewConnection(SocketServer *server)
{
    while (true) {
        SocketClient::SharedPtr client = server->nextConnection();
        if (!client)
            break;
        Connection *conn = new Connection(client);
        conn->newMessage().connect(std::bind(&Server::onNewMessage, this, std::placeholders::_1, std::placeholders::_2));
        conn->disconnected().connect(std::bind(&Server::onConnectionDisconnected, this, std::placeholders::_1));

        if (debugMulti && !conn->client()->peerString().isEmpty()) {
            error() << "Got connection from" << conn->client()->peerString();
        }
    }
}

void Server::onConnectionDisconnected(Connection *o)
{
    o->disconnected().disconnect();
    EventLoop::deleteLater(o);
    mClients.remove(o);
    mPendingJobRequests.remove(o);
}

void Server::onNewMessage(Message *message, Connection *connection)
{
    if (mOptions.unloadTimer)
        mUnloadTimer.restart(mOptions.unloadTimer * 1000 * 60, Timer::SingleShot);

    RTagsMessage *m = static_cast<RTagsMessage*>(message);

    switch (message->messageId()) {
    case CompileMessage::MessageId:
        handleCompileMessage(static_cast<CompileMessage&>(*m), connection);
        break;
    case QueryMessage::MessageId:
        error() << m->raw();
        handleQueryMessage(static_cast<const QueryMessage&>(*m), connection);
        break;
    case IndexerMessage::MessageId:
        handleIndexerMessage(static_cast<const IndexerMessage&>(*m), connection);
        break;
    case LogOutputMessage::MessageId:
        error() << m->raw();
        handleLogOutputMessage(static_cast<const LogOutputMessage&>(*m), connection);
        break;
    case VisitFileMessage::MessageId:
        handleVisitFileMessage(static_cast<const VisitFileMessage&>(*m), connection);
        break;
    case ResponseMessage::MessageId:
    case FinishMessage::MessageId:
    case VisitFileResponseMessage::MessageId:
        error() << getpid() << "Unexpected message" << static_cast<int>(message->messageId());
        // assert(0);
        connection->finish();
        break;
    case JobRequestMessage::MessageId:
        handleJobRequestMessage(static_cast<const JobRequestMessage&>(*m), connection);
        break;
    case JobResponseMessage::MessageId:
        handleJobResponseMessage(static_cast<const JobResponseMessage&>(*m), connection);
        break;
    case ProxyJobAnnouncementMessage::MessageId:
        handleProxyJobAnnouncementMessage(static_cast<const ProxyJobAnnouncementMessage&>(*m), connection);
        break;
    case ClientMessage::MessageId:
        handleClientMessage(static_cast<const ClientMessage&>(*m), connection);
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

void Server::index(const String &arguments, const Path &pwd, const List<String> &withProjects)
{
    Path unresolvedPath;
    Source source = Source::parse(arguments, pwd, &unresolvedPath);
    if (!source.isIndexable())
        return;
    Path project = findProject(source.sourceFile(), unresolvedPath, withProjects);
    if (!shouldIndex(source, project))
        return;

    preprocess(std::move(source), std::move(project), IndexerJob::Compile);
}

void Server::preprocess(Source &&source, Path &&srcRoot, uint32_t flags)
{
    std::shared_ptr<Project> project = mProjects.value(srcRoot);
    if (!project) {
        project = addProject(srcRoot);
        assert(project);
    }
    project->load();

    if (!(mOptions.options & NoJobServer)) {
        std::shared_ptr<PreprocessJob> job(new PreprocessJob(std::move(source), project, flags));
        mPendingPreprocessJobs.append(job);
        startPreprocessJobs();
    } else {
        std::shared_ptr<Cpp> cpp(new Cpp);
        cpp->flags = Cpp::Preprocess_None;
        cpp->time = time(0);
        cpp->preprocessDuration = 0;
        index(std::move(source), cpp, project, flags);

    }
}

void Server::handleCompileMessage(CompileMessage &message, Connection *conn)
{
    conn->finish();
    index(message.arguments(), message.workingDirectory(), message.projects());
}

void Server::handleLogOutputMessage(const LogOutputMessage &message, Connection *conn)
{
    new LogObject(conn, message.level());
}

void Server::handleIndexerMessage(const IndexerMessage &message, Connection *conn)
{
    std::shared_ptr<IndexData> indexData = message.data();
    // error() << "Got indexer message" << message.project() << Location::path(indexData->fileId);
    assert(indexData);
    auto it = mProcessingJobs.find(indexData->jobId);
    if (debugMulti)
        error() << "got indexer message for job" << Location::path(indexData->fileId()) << indexData->jobId
                << "from" << (conn->client()->peerString().isEmpty() ? String("ourselves") : conn->client()->peerString());
    if (it != mProcessingJobs.end()) {
        std::shared_ptr<IndexerJob> job = it->second;
        assert(job);
        mProcessingJobs.erase(it);
        assert(!(job->flags & IndexerJob::FromRemote));

        const String ip = conn->client()->peerName();
        if (!ip.isEmpty())
            indexData->message << String::format<64>(" from %s", ip.constData());

        const IndexerJob::Flag runningFlag = (ip.isEmpty() ? IndexerJob::RunningLocal : IndexerJob::Remote);
        job->flags &= ~runningFlag;

        // we only care about the first job that returns
        if (!(job->flags & (IndexerJob::CompleteLocal|IndexerJob::CompleteRemote))) {
            if (!(job->flags == IndexerJob::Aborted))
                job->flags |= (ip.isEmpty() ? IndexerJob::CompleteRemote : IndexerJob::CompleteLocal);
            std::shared_ptr<Project> project = mProjects.value(message.project());
            if (!project) {
                error() << "Can't find project root for this IndexerMessage" << message.project() << Location::path(indexData->fileId());
            } else {
                project->onJobFinished(indexData);
            }
        }
        if (!(mOptions.options & NoJobServer))
            startPreprocessJobs();
    } else {
        // job already processed
        if (debugMulti)
            error() << "already got a response for" << indexData->jobId;
    }
    conn->finish();
    if (!(mOptions.options & NoJobServer))
        startPreprocessJobs();
    startNextJob();
}

void Server::handleQueryMessage(const QueryMessage &message, Connection *conn)
{
    conn->setSilent(message.flags() & QueryMessage::Silent);
    updateProject(message.projects(), message.flags());

    switch (message.type()) {
    case QueryMessage::Invalid:
        assert(0);
        break;
    case QueryMessage::Sources:
        sources(message, conn);
        break;
    case QueryMessage::SendDiagnostics:
        sendDiagnostics(message, conn);
        break;
    case QueryMessage::CodeCompleteAt:
    case QueryMessage::PrepareCodeCompleteAt:
        codeCompleteAt(message, conn);
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
#if defined(HAVE_CXCOMPILATIONDATABASE) && CLANG_VERSION_MINOR >= 3
    case QueryMessage::LoadCompilationDatabase:
        loadCompilationDatabase(message, conn);
        break;
#endif
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
    }
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
    for (auto it : mProjects) {
        if (it.second->isIndexing()) {
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

    std::shared_ptr<Project> project = updateProjectForLocation(Location::path(fileId));
    if (!project || project->state() != Project::Loaded) {
        conn->write<256>("%s is not indexed", query.query().constData());
        conn->finish();
        return;
    }

    const Source source = project->sources(fileId).value(query.buildIndex());
    if (!source.isNull()) {
        conn->disconnected().disconnect();
        // ### this is a hack, but if the connection goes away we can't post
        // ### events to it. We could fix this nicer but I didn't
        DumpThread *dumpThread = new DumpThread(query, source, conn);
        dumpThread->start();
    } else {
        conn->write<256>("%s build: %d not found", query.query().constData(), query.buildIndex());
        conn->finish();
    }
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
    } else if (project->state() != Project::Loaded) {
        conn->write("Project loading");
        conn->finish();
    } else {
        CursorInfoJob job(loc, query, project);
        job.run(conn);
        conn->finish();
    }
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
    const Source source = project->sources(fileId).value(query.buildIndex());
    if (!source.isValid()) {
        conn->write<256>("%s build: %d not found", query.query().constData(), query.buildIndex());
    } else {
        Preprocessor pre(source, conn);
        pre.preprocess();
    }
    conn->finish();
}

void Server::clearProjects()
{
    for (auto it : mProjects)
        it.second->unload();
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

bool Server::shouldIndex(const Source &source, const Path &srcRoot) const
{
    if (srcRoot.isEmpty())
        return false;
    assert(source.isIndexable());
    if (mOptions.ignoredCompilers.contains(source.compiler()))
        return false;

    const Path sourceFile = source.sourceFile();

    if (Filter::filter(sourceFile, mOptions.excludeFilters) == Filter::Filtered)
        return false;

    std::shared_ptr<Project> project = mProjects.value(srcRoot);
    return !project || !project->hasSource(source);
}

Path Server::findProject(const Path &path, const Path &unresolvedPath, const List<String> &withProjects) const
{
    std::shared_ptr<Project> current = mCurrentProject.lock();
    if (current && (current->match(unresolvedPath) || (path != unresolvedPath && current->match(path))))
        return current->path();

    for (auto it = mProjects.begin(); it != mProjects.end(); ++it) {
        if (it->second->match(unresolvedPath) || (path != unresolvedPath && it->second->match(path)))
            return it->first;
    }

    for (auto it = mProjects.begin(); it != mProjects.end(); ++it) {
        for (auto p = withProjects.begin(); p != withProjects.end(); ++p) {
            if (it->second->match(*p))
                return it->first;
        }
    }

    const Path root = RTags::findProjectRoot(unresolvedPath, RTags::SourceRoot);
    if (root.isEmpty() && path != unresolvedPath)
        return RTags::findProjectRoot(path, RTags::SourceRoot);
    return root;
}

void Server::index(const Source &source, const std::shared_ptr<Cpp> &cpp,
                   const std::shared_ptr<Project> &project, uint32_t flags)
{
    warning() << "Indexing" << source << "in" << project->path();
    if (!mCurrentProject.lock()) {
        mCurrentProject = project;
        setupCurrentProjectFile(project);
    }
    assert(project);
    project->index(source, cpp, flags);
    startNextJob();
}

std::shared_ptr<Project> Server::setCurrentProject(const Path &path, unsigned int queryFlags) // lock always held
{
    auto it = mProjects.find(path);
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

    for (auto it : mProjects) {
        if (it.second != cur && it.second->match(match)) {
            return setCurrentProject(it.second->path());
        }
    }
    return std::shared_ptr<Project>();
}

void Server::removeProject(const QueryMessage &query, Connection *conn)
{
    const bool unload = query.type() == QueryMessage::UnloadProject;

    const Match match = query.match();
    auto it = mProjects.begin();
    while (it != mProjects.end()) {
        auto cur = it++;
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
    const int old = mProjects.size();
    const int cur = reloadProjects();
    conn->write<128>("Changed from %d to %d projects", old, cur);
    conn->finish();
}

bool Server::selectProject(const Match &match, Connection *conn, unsigned int queryFlags)
{
    std::shared_ptr<Project> selected;
    bool error = false;
    for (auto it : mProjects) {
        if (it.second->match(match)) {
            if (error) {
                if (conn)
                    conn->write(it.first);
            } else if (selected) {
                error = true;
                if (conn) {
                    conn->write<128>("Multiple matches for %s", match.pattern().constData());
                    conn->write(selected->path());
                    conn->write(it.first);
                }
                selected.reset();
            } else {
                selected = it.second;
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
    if (query.query().isEmpty()) {
        const std::shared_ptr<Project> current = mCurrentProject.lock();
        const char *states[] = { "(unloaded)", "(inited)", "(loading)", "(loaded)", "(syncing)" };
        for (auto it : mProjects) {
            conn->write<128>("%s %s%s", it.first.constData(), states[it.second->state()], it.second == current ? " <=" : "");
        }
    } else {
        Path selected;
        bool error = false;
        const Match match = query.match();
        const auto it = mProjects.find(match.pattern());
        bool ok = false;
        unsigned long long index = query.query().toULongLong(&ok);
        if (it != mProjects.end()) {
            selected = it->first;
        } else {
            for (auto pit : mProjects) {
                assert(pit.second);
                if (ok) {
                    if (!index) {
                        selected = pit.first;
                    } else {
                        --index;
                    }
                }
                if (pit.second->match(match)) {
                    if (error) {
                        conn->write(pit.first);
                    } else if (!selected.isEmpty()) {
                        error = true;
                        conn->write<128>("Multiple matches for %s", match.pattern().constData());
                        conn->write(selected);
                        conn->write(pit.first);
                        selected.clear();
                    } else {
                        selected = pit.first;
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
    if (query.query().isEmpty()) {
        conn->write<128>("Running with %d jobs", mOptions.jobCount);
    } else {
        const int jobCount = query.query().toLongLong();
        if (jobCount < 0 || jobCount > 100) {
            conn->write<128>("Invalid job count %s (%d)", query.query().constData(), jobCount);
        } else {
            mOptions.jobCount = jobCount;
            if (mThreadPool)
                mThreadPool->setConcurrentJobs(std::max(1, jobCount));
            conn->write<128>("Changed jobs to %d", jobCount);
        }
    }
    conn->finish();
    if (!(mOptions.options & NoJobServer))
        startPreprocessJobs();
    startNextJob();
}

void Server::sendDiagnostics(const QueryMessage &query, Connection *conn)
{
    if (testLog(RTags::CompilationErrorXml))
        logDirect(RTags::CompilationErrorXml, query.query());
    conn->finish();
}

void Server::clearProjects(const QueryMessage &query, Connection *conn)
{
    clearProjects();
    conn->write("Cleared projects");
    conn->finish();
}

#if defined(HAVE_CXCOMPILATIONDATABASE) && CLANG_VERSION_MINOR >= 3
void Server::loadCompilationDatabase(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    // ### this will ignore the actual file name, not sure how to fix that
    CXCompilationDatabase_Error err;
    CXCompilationDatabase db = clang_CompilationDatabase_fromDirectory(path.constData(), &err);
    if (err != CXCompilationDatabase_NoError) {
        conn->write("Can't load compilation database");
        conn->finish();
        return;
    }
    CXCompileCommands cmds = clang_CompilationDatabase_getAllCompileCommands(db);
    const unsigned int sz = clang_CompileCommands_getSize(cmds);
    for (unsigned int i = 0; i < sz; ++i) {
        CXCompileCommand cmd = clang_CompileCommands_getCommand(cmds, i);
        String args;
        CXString str = clang_CompileCommand_getDirectory(cmd);
        Path dir = clang_getCString(str);
        clang_disposeString(str);
        const unsigned int num = clang_CompileCommand_getNumArgs(cmd);
        for (unsigned int j = 0; j < num; ++j) {
            str = clang_CompileCommand_getArg(cmd, j);
            args += clang_getCString(str);
            clang_disposeString(str);
            if (j < num - 1)
                args += " ";
        }

        index(args, dir, query.projects());
    }
    clang_CompileCommands_dispose(cmds);
    clang_CompilationDatabase_dispose(db);
    conn->write("Compilation database loaded");
    conn->finish();
}
#endif

void Server::shutdown(const QueryMessage &query, Connection *conn)
{
    for (auto it : mProjects) {
        if (it.second)
            it.second->unload();
    }
    EventLoop::eventLoop()->quit();
    conn->write("Shutting down");
    conn->finish();
}

void Server::sources(const QueryMessage &query, Connection *conn)
{
    const Path path = query.query();
    if (path.isFile()) {
        std::shared_ptr<Project> project = updateProjectForLocation(path);
        if (project) {
            if (project->state() != Project::Loaded) {
                conn->write("Project loading");
            } else {
                const uint32_t fileId = Location::fileId(path);
                if (fileId) {
                    const List<Source> sources = project->sources(fileId);
                    for (auto it : sources) {
                        if (query.flags() & QueryMessage::CompilationFlagsOnly) {
                            conn->write<128>("%s: %s",
                                             it.sourceFile().constData(),
                                             String::join(it.toCommandLine(0), ' ').constData());
                        } else {
                            conn->write(it.toString());
                        }
                    }
                }
            }
            conn->finish();
            return;
        }
    }

    if (std::shared_ptr<Project> project = currentProject()) {
        const Match match = query.match();
        if (project->state() != Project::Loaded) {
            conn->write("Project loading");
        } else {
            const SourceMap infos = project->sources();
            for (auto it : infos) {
                if (match.isEmpty() || match.match(it.second.sourceFile())) {
                    if (query.flags() & QueryMessage::CompilationFlagsOnly) {
                        conn->write<128>("%s: %s",
                                         it.second.sourceFile().constData(),
                                         String::join(it.second.toCommandLine(0), ' ').constData());
                    } else {
                        conn->write(it.second.toString());
                    }
                }
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
                for (auto it : suspendedFiles)
                    conn->write<512>("%s is suspended", Location::path(it).constData());
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

void Server::handleJobRequestMessage(const JobRequestMessage &message, Connection *conn)
{
    if (debugMulti)
        error() << "got a request for" << message.numJobs() << "jobs";
    int cnt = message.numJobs();
    auto it = mPending.begin();
    while (it != mPending.end()) {
        std::shared_ptr<IndexerJob>& job = *it;
        if (job->flags & (IndexerJob::CompleteLocal|IndexerJob::CompleteRemote)) {
            it = mPending.erase(it);
            continue;
        }

        if (!(job->flags & IndexerJob::FromRemote)) {
            assert(!job->process);
#warning "we shouldn't update state for this job until we've SUCCESSFULLY sent it the receiver"

            job->flags |= IndexerJob::Remote;
            job->flags &= ~IndexerJob::Rescheduled;
            if (!(mOptions.options & NoCompression) && !(job->cpp->flags & Cpp::Preprocess_Compressed)) {
                StopWatch sw;
                job->cpp->preprocessed = job->cpp->preprocessed.compress();
                job->cpp->flags |= Cpp::Preprocess_Compressed;
                if (debugMulti)
                    error() << "Compressed" << job->sourceFile << "in" << sw.elapsed() << "ms";
            }
            mProcessingJobs[job->id] = job;
            if (debugMulti)
                error() << "sending job" << job->sourceFile << "to" << conn->client()->peerString();
            conn->send(JobResponseMessage(job, mOptions.tcpPort));
            conn->sendFinished().connect([job](Connection*) {
                    job->started = Rct::monoMs();
                    if (debugMulti)
                        error() << "Sent job" << job->sourceFile;
                });
            it = mPending.erase(it);
            if (!--cnt)
                break;
            continue;
        }
        ++it;
    }
    conn->finish();
}

void Server::handleJobResponseMessage(const JobResponseMessage &message, Connection *conn)
{
    std::shared_ptr<IndexerJob> job(new IndexerJob);
    message.toIndexerJob(job, conn);
    if (debugMulti) {
        error() << "got indexer job for" << job->destination << ":" << job->port
                << "with preprocessed" << job->cpp->preprocessed.size()
                << job->sourceFile;
    }
    assert(job->flags & IndexerJob::FromRemote);
    addJob(job);
}

void Server::handleJobAnnouncementMessage(const JobAnnouncementMessage &message)
{
    const int available = availableJobSlots(Remote);
    if (debugMulti) {
        error("available jobs %d available %d local %d pending %d processcount %d from %s",
              available, availableJobSlots(Remote), mLocalJobs.size(), mPendingJobRequests.size(),
              mOptions.jobCount, message.host().constData());
    }
    const int jobs = std::min(available, message.numJobs());
    if (jobs) {
        fetchRemoteJobs(message.host(), message.port(), jobs);
    }
}

void Server::handleProxyJobAnnouncementMessage(const ProxyJobAnnouncementMessage &message, Connection *conn)
{
    const JobAnnouncementMessage msg(message.numJobs(), conn->client()->peerName(), message.port());
    if (debugMulti) {
        error() << "Sending proxy job announcement" << conn->client()->peerName() << message.port() << message.numJobs();
    }

    for (auto client : mClients) {
        if (client != conn)
            client->send(msg);
    }
    handleJobAnnouncementMessage(msg);
}

void Server::handleClientMessage(const ClientMessage &, Connection *conn)
{
    error() << "Got a client connected from" << conn->client()->peerString();
    mClients.insert(conn);
}

void Server::handleVisitFileMessage(const VisitFileMessage &message, Connection *conn)
{
    uint32_t fileId = 0;
    bool visit = false;

    std::shared_ptr<Project> project = mProjects.value(message.project());
    Path resolved;
    const uint64_t key = message.key();
    if (project && project->isValidJob(key)) {
        resolved = message.file().resolved();
        fileId = Location::insertFile(resolved);
        visit = project->visitFile(fileId, key);
    }
    VisitFileResponseMessage msg(fileId, resolved, visit);
    conn->send(msg);
}

void Server::restoreFileIds()
{
    const Path p = mOptions.dataDir + "fileids";
    bool clear = true;
    const String all = p.readAll();
    if (!all.isEmpty()) {
        Hash<Path, uint32_t> pathsToIds;
        Deserializer in(all);
        int version;
        in >> version;
        if (version == DatabaseVersion) {
            int size;
            in >> size;
            if (size != all.size()) {
                error("Refusing to load corrupted file %s", p.constData());
            } else {
                in >> pathsToIds;
                clear = false;
                Location::init(pathsToIds);
            }
        } else {
            error("%s has the wrong format. Got %d, expected %d. Can't restore anything",
                  p.constData(), version, Server::DatabaseVersion);
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
    const Hash<Path, uint32_t> pathsToIds = Location::pathsToIds();
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
    std::shared_ptr<Project> cur = mCurrentProject.lock();
    for (auto it : mProjects) {
        if (it.second->state() != Project::Unloaded && it.second != cur && !it.second->isIndexing()) {
            it.second->unload();
        }
    }
}

template <typename T>
static inline bool slowContains(const LinkedList<T> &list, const T &t)
{
    for (T i : list) {
        if (i == t)
            return true;
    }
    return false;
}

void Server::onReschedule()
{
    const uint64_t now = Rct::monoMs();
    auto it = mProcessingJobs.begin();
    while (it != mProcessingJobs.end()) {
        const std::shared_ptr<IndexerJob>& job = it->second;
        assert(!(job->flags & (IndexerJob::CompleteRemote|IndexerJob::CompleteLocal)));
        if (!(job->flags & (IndexerJob::Rescheduled|IndexerJob::RunningLocal))
            && job->flags & IndexerJob::Remote
            && static_cast<int>(now - job->started) >= mOptions.rescheduleTimeout) {
            assert(!job->process);
            // this might never happen, reschedule this job
            // don't take it out of the mProcessingJobs list since the result might come back still
            // if (debugMulti)
            error() << "rescheduling job" << job->sourceFile << job->id
                    << "it's been" << static_cast<double>(now - job->started) / 1000.0 << "seconds";
            job->flags |= IndexerJob::Rescheduled;
            assert(!slowContains(mPending, job));
            mPending.push_back(job);
            startNextJob();
        }
        ++it;
    }
}

void Server::onMulticastReadyRead(const SocketClient::SharedPtr &socket,
                                  const String &ip,
                                  uint16_t port,
                                  Buffer &&in)
{
    const Buffer buffer = std::forward<Buffer>(in);
    if (debugMulti)
        error() << "Got some data from multicast socket" << buffer.size();
    const char *data = reinterpret_cast<const char*>(buffer.data());
    if (data[0] != 's') {
        error() << "Got unexpected multicast message";
        return;
    } else if (buffer.size() == 2 && data[1] == '?') { // query for server
        if (!mServerConnection && !(mOptions.options & JobServer))
            return;

        String data;
        Serializer serializer(data);
        serializer.write("s", 1);
        if (mOptions.jobServer.second) {
            serializer << mOptions.jobServer.first << mOptions.jobServer.second;
        } else {
            serializer << String() << mOptions.tcpPort;
        }
        if (debugMulti)
            error() << ip << "wants to know about the server" << mOptions.jobServer << mOptions.tcpPort;
        mMulticastSocket->writeTo(mOptions.multicastAddress, mOptions.multicastPort,
                                  reinterpret_cast<const unsigned char*>(data.constData()), data.size());
    } else if (!(mOptions.options & JobServer) && !mOptions.jobServer.second) { // looking for server
        Deserializer deserializer(data + 1, buffer.size() - 1);
        deserializer >> mOptions.jobServer.first >> mOptions.jobServer.second;
        if (mOptions.jobServer.first.isEmpty())
            mOptions.jobServer.first = ip;
        if (debugMulti)
            error() << ip << "told about the server at" << mOptions.jobServer;
        connectToServer();
    }
}

inline int Server::availableJobSlots(JobSlotsMode mode) const
{
    const int count = std::max(mOptions.jobCount - (mThreadPool ? mThreadPool->busyThreads() : 0), 0);
    if (mode == Local)
        return count;
    int ret = mLocalJobs.size();
    for (auto it : mPendingJobRequests) {
        ret += it.second;
    }
    return std::max(0, count - ret);
}

void Server::fetchRemoteJobs(const String& ip, uint16_t port, uint16_t jobs)
{
    if (debugMulti)
        error() << "connecting to" << ip << port;
    // Could cache these connections. We could at least use the server
    // connection if it's the server we're connecting to.
    Connection* conn = new Connection;
    if (!conn->connectTcp(ip, port)) {
        delete conn;
        return;
    }
    if (debugMulti)
        error() << "asking for" << jobs << "jobs";
    conn->newMessage().connect(std::bind(&Server::onNewMessage, this, std::placeholders::_1, std::placeholders::_2));
    conn->disconnected().connect(std::bind(&Server::onConnectionDisconnected, this, std::placeholders::_1));
    conn->finished().connect(std::bind([this, conn]() { mPendingJobRequests.remove(conn); conn->close(); EventLoop::deleteLater(conn); }));
    conn->send(JobRequestMessage(jobs));
    assert(!mPendingJobRequests.contains(conn));
    mPendingJobRequests[conn] = jobs;
}

void Server::addJob(const std::shared_ptr<IndexerJob> &job)
{
    warning() << "adding job" << job->sourceFile;
    assert(job);
    assert(!(job->flags & (IndexerJob::CompleteRemote|IndexerJob::CompleteLocal)));
    mPending.push_back(job);
    if (job->flags & IndexerJob::FromRemote)
        ++mRemotePending;
    startNextJob();
}

void Server::startNextJob()
{
    while (!mPending.isEmpty() && mLocalJobs.size() < availableJobSlots(Local)) {
        std::shared_ptr<IndexerJob> job = mPending.first();
        assert(job);
        if (!(job->flags & (IndexerJob::CompleteLocal|IndexerJob::CompleteRemote))) {
            if (job->flags & IndexerJob::FromRemote || project(job->project)) {
                if (!(job->flags & IndexerJob::FromRemote))
                    mProcessingJobs[job->id] = job;
                job->flags &= ~IndexerJob::Rescheduled;
                if (job->launchProcess()) {
                    if (debugMulti)
                        error() << "started job locally for" << job->sourceFile << job->id;
                    mLocalJobs[job->process] = std::make_pair(job, Rct::monoMs());
                    assert(job->process);
                    job->process->finished().connect(std::bind(&Server::onLocalJobFinished, this,
                                                               std::placeholders::_1));
                } else {
                    mLocalJobs[job->process] = std::make_pair(job, Rct::monoMs());
                    EventLoop::eventLoop()->callLater(std::bind(&Server::onLocalJobFinished, this, std::placeholders::_1), job->process);
                }
            }
        }
        mPending.pop_front();
    }
    if (mOptions.options & NoJobServer)
        return;

    const int jobs = mPending.size() - mRemotePending;
    if ((!(mOptions.options & JobServer) && !mServerConnection) || !jobs || jobs == mLastJobAnnouncementCount || mPending.empty()) {
        mLastJobAnnouncementCount = jobs;
        return;
    }
    mLastJobAnnouncementCount = jobs;
    if (mServerConnection) {
        mServerConnection->send(ProxyJobAnnouncementMessage(jobs, mOptions.tcpPort));
    } else {
        const JobAnnouncementMessage msg(mPending.size() - mRemotePending, mHostName, mOptions.tcpPort);
        for (auto client : mClients) {
            client->send(msg);
        }
    }
    if (debugMulti)
        error() << "announcing" << mPending.size() - mRemotePending << "jobs";
}

void Server::onLocalJobFinished(Process *process)
{
    assert(process);
    auto it = mLocalJobs.find(process);
    assert(it != mLocalJobs.end());
    std::shared_ptr<IndexerJob> &job = it->second.first;
    assert(job->process == process);
    error() << process->readAllStdErr() << process->readAllStdOut();
    if (debugMulti)
        error() << "job finished" << job->flags << process->errorString() << process->readAllStdErr();
    if (job->flags & IndexerJob::FromRemote) {
        --mRemotePending;
        error() << "Built remote job" << job->sourceFile.toTilde() << "for"
                << job->destination
                << "in" << (Rct::monoMs() - it->second.second) << "ms";
    }
    if (!(job->flags & (IndexerJob::CompleteRemote|IndexerJob::CompleteLocal))
        && (process->returnCode() != 0 || !process->errorString().isEmpty())) {
        if (!(job->flags & IndexerJob::Aborted))
            job->flags |= IndexerJob::Crashed;
        job->flags &= ~IndexerJob::RunningLocal;

        std::shared_ptr<Project> proj = project(job->project);
        if (proj && (proj->state() == Project::Loaded || proj->state() == Project::Syncing)) {
            std::shared_ptr<IndexData> data(new IndexData(job->flags));
            data->key = job->source.key();
            EventLoop::SharedPtr loop = EventLoop::eventLoop();
            assert(loop);
            loop->callLater([proj, data]() { proj->onJobFinished(data); });
        }
    }
    job->process = 0;
    mProcessingJobs.erase(job->id);
    mLocalJobs.erase(it);
    EventLoop::deleteLater(process);
    if (!(mOptions.options & NoJobServer))
        startPreprocessJobs();
    startNextJob();
}

void Server::stopServers()
{
    Path::rm(mOptions.socketFile);
    mUnixServer.reset();
    mTcpServer.reset();
    mHttpServer.reset();
    mProjects.clear();
}

static inline uint64_t connectTime(uint64_t lastAttempt, int failures)
{
    uint64_t wait = 0;
    if (failures) {
        wait = 1000;
        for (int i=1; i<failures; ++i) {
            wait *= 2;
        }
    }
    return lastAttempt + wait;
}

void Server::codeCompleteAt(const QueryMessage &query, Connection *conn)
{
    const String q = query.query();
    Deserializer deserializer(q);
    Path path;
    int line, column;
    deserializer >> path >> line >> column;
    std::shared_ptr<Project> project = updateProjectForLocation(path);
    if (!project) {
        conn->write<128>("No project found for %s", path.constData());
        conn->finish();
        return;
    }
    const uint32_t fileId = Location::insertFile(path);
    const Source source = project->sources(fileId).value(query.buildIndex());
    if (source.isNull()) {
        conn->write<128>("No source found for %s", path.constData());
        conn->finish();
        return;
    }
    if (!mCompletionThread) {
        mCompletionThread = new CompletionThread(10); // ### need setting
        mCompletionThread->start();
    }

    const Location loc(fileId, line, column);
    unsigned int flags = CompletionThread::None;
    if (query.type() == QueryMessage::PrepareCodeCompleteAt)
        flags |= CompletionThread::Refresh;
    if (query.flags() & QueryMessage::ElispList)
        flags |= CompletionThread::Elisp;
    mCompletionThread->completeAt(source, loc, flags, query.unsavedFiles().value(path));
    conn->finish();
    error() << "Got completion" << query.type() << path << line << column;
}

int Server::startPreprocessJobs()
{
    assert(!(mOptions.options & NoJobServer));
    assert(mThreadPool);
    int ret = 0;
    int size = mPending.size() + mThreadPool->backlockSize() + mThreadPool->busyThreads() + mProcessingJobs.size();
    enum { MaxPending = 50 };
    while (size < MaxPending && !mPendingPreprocessJobs.isEmpty()) {
        ++ret;
        ++size;
        std::shared_ptr<PreprocessJob> job = mPendingPreprocessJobs.front();
        mPendingPreprocessJobs.pop_front();
        mThreadPool->start(job);
    }
    return ret;
}

static inline void drain(const SocketClient::SharedPtr &sock)
{
    Buffer data = std::move(sock->takeBuffer());
    (void)data;
}

void Server::onHttpClientReadyRead(const SocketClient::SharedPtr &socket)
{
    auto &log = mHttpClients[socket];
    if (!log) {
        static const char *statsRequestLine = "GET /stats HTTP/1.1\r\n";
        static const size_t statsLen = strlen(statsRequestLine);
        const size_t len = socket->buffer().size();
        if (len >= statsLen) {
            if (!memcmp(socket->buffer().data(), statsRequestLine, statsLen)) {
                static const char *response = ("HTTP/1.1 200 OK\r\n"
                                               "Cache: no-cache\r\n"
                                               "Cache-Control: private\r\n"
                                               "Pragma: no-cache\r\n"
                                               "Content-Type: text/event-stream\r\n\r\n");
                static const int responseLen = strlen(response);
                socket->write(reinterpret_cast<const unsigned char*>(response), responseLen);
                log.reset(new HttpLogObject(RTags::Statistics, socket));
                ::drain(socket);
            } else {
                socket->close();
            }
        }
    } else {
        ::drain(socket);
    }
}

void Server::connectToServer()
{
    warning() << "connectToServer";
    mConnectToServerTimer.stop();
    assert(!(mOptions.options & JobServer));
    if (mServerConnection)
        return;
    enum { ServerReconnectTimer = 5000 };
    if (!mOptions.jobServer.second) {
        if (mMulticastSocket) {
            const unsigned char query[2] = { 's', '?' };
            mMulticastSocket->writeTo(mOptions.multicastAddress, mOptions.multicastPort, query, 2);
            mConnectToServerTimer.restart(ServerReconnectTimer);
        }
    } else {
        mServerConnection = new Connection;
        mServerConnection->disconnected().connect([this](Connection *conn) {
                assert(conn == mServerConnection);
                (void)conn;
                EventLoop::deleteLater(mServerConnection);
                mServerConnection = 0;
                mConnectToServerTimer.restart(ServerReconnectTimer);
                warning() << "Disconnected from server" << conn->client()->peerName();
            });
        mServerConnection->connected().connect([this](Connection *conn) {
                assert(conn == mServerConnection);
                (void)conn;
                if (!mServerConnection->send(ClientMessage())) {
                    mServerConnection->close();
                    EventLoop::deleteLater(mServerConnection);
                    mServerConnection = 0;
                    mConnectToServerTimer.restart(ServerReconnectTimer);
                    error() << "Couldn't send logoutputmessage";
                } else {
                    error() << "Connected to server" << conn->client()->peerName();
                }
            });
        mServerConnection->newMessage().connect([this](Message *msg, Connection *conn) {
                assert(msg->messageId() == JobAnnouncementMessage::MessageId);
                handleJobAnnouncementMessage(static_cast<const JobAnnouncementMessage&>(*msg));
            });

        if (!mServerConnection->connectTcp(mOptions.jobServer.first, mOptions.jobServer.second)) {
            delete mServerConnection;
            mServerConnection = 0;
            mConnectToServerTimer.restart(ServerReconnectTimer);
            error() << "Failed to connect to server" << mOptions.jobServer;
        }
    }
}

void Server::dumpJobs(Connection *conn)
{
    if (!mPending.isEmpty()) {
        conn->write("Pending:");
        for (auto job : mPending) {
            conn->write<128>("%s: 0x%x", job->sourceFile.constData(), job->flags);
        }
    }
    if (!mLocalJobs.isEmpty()) {
        conn->write("Local:");
        for (auto job : mLocalJobs) {
            conn->write<128>("%s: 0x%x", job.second.first->sourceFile.constData(), job.second.first->flags);
        }
    }
    if (!mProcessingJobs.isEmpty()) {
        conn->write("Processing:");
        for (auto job : mProcessingJobs) {
            conn->write<128>("%s: 0x%x", job.second->sourceFile.constData(), job.second->flags);
        }
    }
}
