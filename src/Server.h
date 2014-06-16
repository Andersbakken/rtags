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

class CompileMessage;
class CompletionThread;
class Connection;
class ErrorMessage;
class IndexerMessage;
class Job;
class JobOutput;
class LogOutputMessage;
class Message;
class OutputMessage;
class Project;
class QueryMessage;
class VisitFileMessage;
class Server
{
public:
    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x0000,
        ClearProjects = 0x0001,
        Wall = 0x0002,
        IgnorePrintfFixits = 0x0004,
        UnlimitedErrors = 0x0008,
        SpellChecking = 0x0010,
        DisallowMultipleSources = 0x0020,
        NoStartupCurrentProject = 0x0040,
        WatchSystemPaths = 0x0080,
        NoFileManagerWatch = 0x0100,
        NoLocalCompiles = 0x0200,
        NoNoUnknownWarningsOption = 0x0400,
        SuspendRPOnCrash = 0x0800,
        SeparateDebugAndRelease = 0x1000
    };
    struct Options {
        Options()
            : options(0), jobCount(0), unloadTimer(0),
              rpVisitFileTimeout(0), rpIndexerMessageTimeout(0), rpConnectTimeout(0),
              syncThreshold(0), threadStackSize(0), maxCrashCount(0), completionCacheSize(0)
        {}
        Path socketFile, dataDir;
        unsigned options;
        int jobCount, unloadTimer, rpVisitFileTimeout,
            rpIndexerMessageTimeout, rpConnectTimeout, syncThreshold,
            threadStackSize, maxCrashCount, completionCacheSize;
        List<String> defaultArguments, excludeFilters;
        List<Path> includePaths;
        List<Source::Define> defines;
        Set<Path> ignoredCompilers;
        List<std::pair<RegExp, Source::Language> > extraCompilers;
    };
    bool init(const Options &options);
    const Options &options() const { return mOptions; }
    bool saveFileIds() const;
    void onJobOutput(JobOutput&& out);
    void addJob(const std::shared_ptr<IndexerJob> &job);
    std::shared_ptr<Project> project(const Path &path) const { return mProjects.value(path); }
    // void index(const Source &source, const Path &project, uint32_t flags);
    // void index(const std::shared_ptr<Unit> &unit, const std::shared_ptr<Project> &project);
    bool shouldIndex(const Source &source, const Path &project) const;
    void stopServers();
    int mongooseStatistics(struct mg_connection *conn);
    void dumpJobs(Connection *conn);
private:
    void restoreFileIds();
    bool index(const String &arguments, const Path &pwd, const Path &projectRootOverride, bool escape);
    void onNewConnection(SocketServer *server);
    void setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags = 0);
    void onUnload();
    void onNewMessage(Message *message, Connection *conn);
    void onConnectionDisconnected(Connection *o);
    void clearProjects();
    void handleCompileMessage(CompileMessage &message, Connection *conn);
    void handleIndexerMessage(const IndexerMessage &message, Connection *conn);
    void handleQueryMessage(const QueryMessage &message, Connection *conn);
    void handleErrorMessage(const ErrorMessage &message, Connection *conn);
    void handleLogOutputMessage(const LogOutputMessage &message, Connection *conn);
    void handleVisitFileMessage(const VisitFileMessage &message, Connection *conn);

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
    void onLocalJobFinished(Process *process);
    bool hasServer() const;
    void onHttpClientReadyRead(const SocketClient::SharedPtr &socket);
    void connectToServer();
    void startJobs();

    typedef Hash<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    SocketServer::SharedPtr mUnixServer;
    bool mVerbose;

    Timer mUnloadTimer;

    LinkedList<std::shared_ptr<IndexerJob> > mPendingJobs;
    Hash<uint64_t, std::shared_ptr<IndexerJob> > mActiveJobs;

    CompletionThread *mCompletionThread;
};

#endif
