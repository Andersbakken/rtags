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

#ifndef Server_h
#define Server_h

#include "FileManager.h"
#include "RTagsClang.h"
#include "RTags.h"
#include "Source.h"
#include "IndexerJob.h"
#include "Match.h"
#include <rct/Connection.h>
#include <rct/FileSystemWatcher.h>
#include <rct/List.h>
#include <rct/Hash.h>
#include <rct/String.h>
#include <rct/Timer.h>
#include <rct/SocketServer.h>

class ClientConnectedMessage;
class ClientMessage;
class CompileMessage;
class CompletionThread;
class Connection;
class ErrorMessage;
class ExitMessage;
class HttpLogObject;
class IndexerMessage;
class Job;
class JobAnnouncementMessage;
class JobOutput;
class JobRequestMessage;
class JobResponseMessage;
class LogOutputMessage;
class Message;
class OutputMessage;
class PreprocessJob;
class Project;
class ProxyJobAnnouncementMessage;
class QueryMessage;
class VisitFileMessage;
class Server
{
public:
    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x00000,
        ClearProjects = 0x00001,
        Wall = 0x00002,
        IgnorePrintfFixits = 0x00004,
        UnlimitedErrors = 0x00008,
        SpellChecking = 0x00010,
        DisallowMultipleSources = 0x00020,
        NoStartupCurrentProject = 0x00040,
        WatchSystemPaths = 0x00080,
        NoFileManagerWatch = 0x00100,
        JobServer = 0x00200,
        NoJobServer = 0x00400,
        CompressionRemote = 0x00800,
        CompressionAlways = 0x01000,
        NoLocalCompiles = 0x02000,
        NoNoUnknownWarningsOption = 0x04000,
        SuspendRPOnCrash = 0x08000,
        SeparateDebugAndRelease = 0x10000,
        ForcePreprocessing = 0x20000
    };
    struct Options {
        Options()
            : options(0), jobCount(0), unloadTimer(0),
              rpVisitFileTimeout(0), rpIndexerMessageTimeout(0), rpConnectTimeout(0),
              syncThreshold(0), rescheduleTimeout(0), multicastTTL(0), threadStackSize(0),
              maxPendingPreprocessSize(0), maxCrashCount(0), completionCacheSize(0),
              tcpPort(0), multicastPort(0), httpPort(0)
        {}
        Path socketFile, dataDir;
        unsigned options;
        int jobCount, unloadTimer, rpVisitFileTimeout,
            rpIndexerMessageTimeout, rpConnectTimeout, syncThreshold,
            rescheduleTimeout, multicastTTL, threadStackSize, maxPendingPreprocessSize,
            maxCrashCount, completionCacheSize;
        List<String> defaultArguments, excludeFilters;
        List<Path> includePaths;
        List<Source::Define> defines;
        String multicastAddress;
        uint16_t tcpPort, multicastPort, httpPort;
        Set<Path> ignoredCompilers;
        List<std::pair<RegExp, Source::Language> > extraCompilers;
        std::pair<String, uint16_t> jobServer;
    };
    bool init(const Options &options);
    const Options &options() const { return mOptions; }
    bool saveFileIds() const;
    void onJobOutput(JobOutput&& out);
    void addJob(const std::shared_ptr<IndexerJob> &job);
    std::shared_ptr<Project> project(const Path &path) const { return mProjects.value(path); }
    void index(const Source &source, const std::shared_ptr<Cpp> &cpp,
               const std::shared_ptr<Project> &project, uint32_t flags);
    void preprocess(Source &&source, Path &&project, uint32_t indexerJobFlags);
    bool shouldIndex(const Source &source, const Path &project) const;
    void stopServers();
    int mongooseStatistics(struct mg_connection *conn);
    void dumpJobs(Connection *conn);
    int exitCode() const { return mExitCode; }
private:
    void restoreFileIds();
    void clear();
    bool index(const String &arguments, const Path &pwd, const Path &projectRootOverride, bool escape);
    void onNewConnection(SocketServer *server);
    void setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags = 0);
    void onUnload();
    void onReschedule();
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDisconnected(Connection *o);
    void clearProjects();
    void handleExitMessage(const ExitMessage &message);
    void handleCompileMessage(CompileMessage &message, Connection *conn);
    void handleIndexerMessage(const IndexerMessage &message, Connection *conn);
    void handleQueryMessage(const QueryMessage &message, Connection *conn);
    void handleErrorMessage(const ErrorMessage &message, Connection *conn);
    void handleLogOutputMessage(const LogOutputMessage &message, Connection *conn);
    void handleVisitFileMessage(const VisitFileMessage &message, Connection *conn);
    void handleJobRequestMessage(const JobRequestMessage &message, Connection *conn);
    void handleJobResponseMessage(const JobResponseMessage &message, Connection *conn);
    void handleJobAnnouncementMessage(const JobAnnouncementMessage &message, Connection *conn);
    void handleClientConnectedMessage(const ClientConnectedMessage &message);
    void handleProxyJobAnnouncementMessage(const ProxyJobAnnouncementMessage &message, Connection *conn);
    void handleClientMessage(const ClientMessage &message, Connection *conn);

    // Queries
    void sendDiagnostics(const QueryMessage &query, Connection *conn);
    void clearProjects(const QueryMessage &query, Connection *conn);
    void codeCompleteAt(const QueryMessage &query, Connection *conn);
    void cursorInfo(const QueryMessage &query, Connection *conn);
    void dependencies(const QueryMessage &query, Connection *conn);
    void dumpFile(const QueryMessage &query, Connection *conn);
    void findFile(const QueryMessage &query, Connection *conn);
    void findSymbols(const QueryMessage &query, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void followLocation(const QueryMessage &query, Connection *conn);
    void hasFileManager(const QueryMessage &query, Connection *conn);
    void isIndexed(const QueryMessage &query, Connection *conn);
    void isIndexing(const QueryMessage &, Connection *conn);
    void jobCount(const QueryMessage &query, Connection *conn);
    void listSymbols(const QueryMessage &query, Connection *conn);
    void preprocessFile(const QueryMessage &query, Connection *conn);
    void project(const QueryMessage &query, Connection *conn);
    void referencesForLocation(const QueryMessage &query, Connection *conn);
    void referencesForName(const QueryMessage &query, Connection *conn);
    void reindex(const QueryMessage &query, Connection *conn);
    void reloadFileManager(const QueryMessage &query, Connection *conn);
    void reloadProjects(const QueryMessage &query, Connection *conn);
    void removeFile(const QueryMessage &query, Connection *conn);
    void removeProject(const QueryMessage &query, Connection *conn);
    void shutdown(const QueryMessage &query, Connection *conn);
    void sources(const QueryMessage &query, Connection *conn);
    void dumpCompletions(const QueryMessage &query, Connection *conn);
    void status(const QueryMessage &query, Connection *conn);
    void syncProject(const QueryMessage &qyery, Connection *conn);
    void suspendFile(const QueryMessage &query, Connection *conn);

    std::shared_ptr<Project> projectForQuery(const QueryMessage &queryMessage);
    std::shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    int reloadProjects();
    std::shared_ptr<Project> addProject(const Path &path);
    void onMulticastReadyRead(const SocketClient::SharedPtr &socket, const String &ip,
                              uint16_t port, Buffer &&buffer);
    void handleMulticastData(const String &ip, uint16_t port, const unsigned char *data, int size, Connection *src);
    void onLocalJobFinished(Process *process);
    bool hasServer() const;
    struct WorkScope {
        WorkScope();
        ~WorkScope();
        bool work;
    };
    void work();
    void onHttpClientReadyRead(const SocketClient::SharedPtr &socket);
    void connectToServer();
    void startRescheduleTimer();

    typedef Hash<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    SocketServer::SharedPtr mUnixServer, mTcpServer, mHttpServer;
    bool mVerbose;

    Timer mUnloadTimer, mRescheduleTimer, mConnectToServerTimer;
    int mConnectToServerFailures;

    LinkedList<std::shared_ptr<IndexerJob> > mPending;
    LinkedList<std::shared_ptr<PreprocessJob> > mPendingPreprocessJobs;
    Hash<uint64_t, std::shared_ptr<IndexerJob> > mProcessingJobs;
    Hash<Process*, std::pair<std::shared_ptr<IndexerJob>, uint64_t> > mLocalJobs;
    ThreadPool *mThreadPool;
    Connection *mServerConnection;
    Hash<SocketClient::SharedPtr, std::shared_ptr<HttpLogObject> > mHttpClients;
    Set<Connection*> mClients;
    SocketClient::SharedPtr mMulticastSocket;

    CompletionThread *mCompletionThread;

    // ### these really should be in the Remote somehow. The problem is that we
    // ### can currently have multiple for the same remote
    Hash<Connection*, uint16_t> mPendingJobRequests;
    struct Remote {
        Remote(const String &h, uint16_t p)
            : next(0), prev(0), host(h), port(p)
        {}
        Remote *next, *prev;
        std::string host;
        uint16_t port;
    } *mFirstRemote, *mLastRemote;
    bool mAnnounced;
    Hash<String, Remote *> mRemotes;
    bool mWorkPending;
    int mExitCode;
};

#endif
