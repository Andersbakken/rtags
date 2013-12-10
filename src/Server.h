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

#include "QueryMessage.h"
#include "CompileMessage.h"
#include "CreateOutputMessage.h"
#include "IndexerMessage.h"
#include "FileManager.h"
#include "QueryMessage.h"
#include "RTagsClang.h"
#include "RTags.h"
#include "Source.h"
#include "IndexerJob.h"
#include <rct/Connection.h>
#include <rct/FileSystemWatcher.h>
#include <rct/List.h>
#include <rct/Hash.h>
#include <rct/String.h>
#include <rct/Timer.h>
#include <rct/SocketServer.h>

class Connection;
class Message;
class ErrorMessage;
class OutputMessage;
class CompileMessage;
class Job;
class JobOutput;
class Project;
class VisitFileMessage;
class JobRequestMessage;
class JobResponseMessage;
class MulticastForwardMessage;
class Server
{
public:
    enum { DatabaseVersion = 35 };

    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x0000,
        NoBuiltinIncludes = 0x0001,
        ClearProjects = 0x0002,
        Wall = 0x0004,
        IgnorePrintfFixits = 0x0008,
        UnlimitedErrors = 0x0010,
        SpellChecking = 0x0020,
        AllowMultipleSources = 0x0040,
        NoStartupCurrentProject = 0x0080,
        WatchSystemPaths = 0x0100,
        NoFileManagerWatch = 0x0200,
        NoEsprima = 0x0400,
        UseCompilerFlags = 0x0800
    };
    struct Options {
        Options()
            : options(0), processCount(0), preprocessCount(0), unloadTimer(0),
              rpVisitFileTimeout(0), rpIndexerMessageTimeout(0), rpConnectTimeout(0),
              syncThreshold(0), rescheduleTimeout(0), multicastTTL(0), tcpPort(0),
              multicastPort(0)
        {}
        Path socketFile, dataDir;
        unsigned options;
        int processCount, preprocessCount, unloadTimer, rpVisitFileTimeout,
            rpIndexerMessageTimeout, rpConnectTimeout, syncThreshold,
            rescheduleTimeout, multicastTTL;
        List<String> defaultArguments, excludeFilters;
        List<Path> includePaths;
        List<Source::Define> defines;
        String multicastAddress;
        uint16_t tcpPort, multicastPort;
        Set<std::pair<String, uint16_t> > multicastForwards;
        Set<Path> ignoredCompilers;
    };
    bool init(const Options &options);
    const Options &options() const { return mOptions; }
    uint32_t currentFileId() const { return mCurrentFileId; }
    bool saveFileIds() const;
    void onJobOutput(JobOutput&& out);
    void startJob(const std::shared_ptr<IndexerJob> &job);
    std::shared_ptr<Project> project(const Path &path) const { return mProjects.value(path); }
    void index(const Source &source, const std::shared_ptr<Cpp> &cpp,
               const Path &project, IndexerJob::IndexType type);
    void preprocess(Source &&source, Path &&project, IndexerJob::IndexType type);
    bool shouldIndex(const Source &source, const Path &project) const;
    Path findProject(const Path &path, const Path &unresolvedPath, const List<String> &withProjects) const;
    void stopServers();
private:
    bool selectProject(const Match &match, Connection *conn, unsigned int queryFlags);
    bool updateProject(const List<String> &projects, unsigned int queryFlags);
    void restoreFileIds();
    void clear();
    void index(const String &arguments, const Path &pwd, const List<String> &withProjects);
    void onNewConnection(SocketServer *server);
    std::shared_ptr<Project> setCurrentProject(const Path &path, unsigned int queryFlags = 0);
    std::shared_ptr<Project> setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags = 0);
    void onUnload();
    void onReschedule();
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDisconnected(Connection *o);
    void clearProjects();
    void handleCompileMessage(CompileMessage &message, Connection *conn);
    void handleIndexerMessage(const IndexerMessage &message, Connection *conn);
    void handleQueryMessage(const QueryMessage &message, Connection *conn);
    void handleErrorMessage(const ErrorMessage &message, Connection *conn);
    void handleCreateOutputMessage(const CreateOutputMessage &message, Connection *conn);
    void handleVisitFileMessage(const VisitFileMessage &message, Connection *conn);
    void handleJobRequestMessage(const JobRequestMessage &message, Connection *conn);
    void handleJobResponseMessage(const JobResponseMessage &message, Connection *conn);
    void handleMulticastForward(const QueryMessage &message, Connection *conn);
    void handleMulticastForwardMessage(const MulticastForwardMessage &message, Connection *conn);
    void isIndexing(const QueryMessage &, Connection *conn);
    void removeFile(const QueryMessage &query, Connection *conn);
    void followLocation(const QueryMessage &query, Connection *conn);
    void cursorInfo(const QueryMessage &query, Connection *conn);
    void dependencies(const QueryMessage &query, Connection *conn);
    void fixIts(const QueryMessage &query, Connection *conn);
    void jobCount(const QueryMessage &query, Connection *conn);
    void referencesForLocation(const QueryMessage &query, Connection *conn);
    void referencesForName(const QueryMessage &query, Connection *conn);
    void findSymbols(const QueryMessage &query, Connection *conn);
    void listSymbols(const QueryMessage &query, Connection *conn);
    void status(const QueryMessage &query, Connection *conn);
    void isIndexed(const QueryMessage &query, Connection *conn);
    void hasFileManager(const QueryMessage &query, Connection *conn);
    void reloadFileManager(const QueryMessage &query, Connection *conn);
    void preprocessFile(const QueryMessage &query, Connection *conn);
    void findFile(const QueryMessage &query, Connection *conn);
    void dumpFile(const QueryMessage &query, Connection *conn);
    void removeProject(const QueryMessage &query, Connection *conn);
    void reloadProjects(const QueryMessage &query, Connection *conn);
    void project(const QueryMessage &query, Connection *conn);
    void clearProjects(const QueryMessage &query, Connection *conn);
    void loadCompilationDatabase(const QueryMessage &query, Connection *conn);
    void shutdown(const QueryMessage &query, Connection *conn);
    void sources(const QueryMessage &query, Connection *conn);
    void suspendFile(const QueryMessage &query, Connection *conn);
    void reindex(const QueryMessage &query, Connection *conn);
    std::shared_ptr<Project> updateProjectForLocation(const Match &match);
    void setupCurrentProjectFile(const std::shared_ptr<Project> &project);
    std::shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    int reloadProjects();
    std::shared_ptr<Project> addProject(const Path &path);
    bool connectMulticastForward(const std::pair<String, uint16_t> &host);
    void onMulticastReadyRead(const SocketClient::SharedPtr &socket, const String &ip,
                              uint16_t port, Buffer &&buffer);
    void handleMulticastData(const String &ip, uint16_t port, const unsigned char *data, int size, Connection *src);
    void onMulticastForwardError(const SocketClient::SharedPtr &socket, SocketClient::Error);
    void onLocalJobFinished(Process *process);
    void startNextJob();
    void fetchRemoteJobs(const String& ip, uint16_t port, uint16_t jobs);

    int availableJobSlots() const;

    typedef Hash<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    SocketServer::SharedPtr mUnixServer, mTcpServer;
    bool mVerbose;

    Timer mUnloadTimer, mRescheduleTimer;

    uint32_t mCurrentFileId;
    std::shared_ptr<SocketClient> mMulticastSocket;
    LinkedList<std::shared_ptr<IndexerJob> > mPending;
    Hash<uint64_t, std::shared_ptr<IndexerJob> > mProcessingJobs;
    Hash<Process*, std::shared_ptr<IndexerJob> > mLocalJobs;
    Hash<Connection*, uint16_t> mPendingJobRequests;
    ThreadPool *mThreadPool;
    unsigned int mRemotePending;

    Map<std::pair<String, uint16_t>, Connection*> mMulticastForwards;
};

#endif
