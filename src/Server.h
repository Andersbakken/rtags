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
class QueryJob;
class JobOutput;
class LogOutputMessage;
class Message;
class OutputMessage;
class Project;
class QueryMessage;
class VisitFileMessage;
class JobScheduler;
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
        NoUnlimitedErrors = 0x00008,
        SpellChecking = 0x00010,
        DisallowMultipleSources = 0x00020,
        NoStartupCurrentProject = 0x00040,
        WatchSystemPaths = 0x00080,
        NoFileManagerWatch = 0x00100,
        NoFileSystemWatch = 0x00200,
        NoNoUnknownWarningsOption = 0x00400,
        SuspendRPOnCrash = 0x00800,
        SeparateDebugAndRelease = 0x01000,
        AllowPedantic = 0x02000,
        StartSuspended = 0x04000,
        EnableCompilerManager = 0x08000,
        EnableNDEBUG = 0x10000,
        NoProgress = 0x20000
    };
    struct Options {
        Options()
            : options(0), jobCount(0), unloadTimer(0),
              rpVisitFileTimeout(0), rpIndexerMessageTimeout(0), rpConnectTimeout(0),
              rpNiceValue(0), syncThreshold(0), threadStackSize(0), maxCrashCount(0),
              completionCacheSize(0), astCache(0)
        {}
        Path socketFile, dataDir;
        unsigned options;
        int jobCount, unloadTimer, rpVisitFileTimeout,
            rpIndexerMessageTimeout, rpConnectTimeout, rpNiceValue,
            syncThreshold, threadStackSize, maxCrashCount, completionCacheSize, astCache;
        List<String> defaultArguments, excludeFilters;
        Set<String> blockedArguments;
        List<Source::Include> includePaths;
        List<Source::Define> defines;
        Set<Path> ignoredCompilers;
        List<std::pair<RegExp, Source::Language> > extraCompilers;

        inline bool flag(enum Option o) const { return 0 != (options & o); }
    };
    bool init(const Options &options);
    const Options &options() const { return mOptions; }
    bool suspended() const { return mSuspended; }
    bool saveFileIds();
    void onJobOutput(JobOutput&& out);
    std::shared_ptr<Project> project(const Path &path) const { return mProjects.value(path); }
    bool shouldIndex(const Source &source, const Path &project) const;
    void stopServers();
    int mongooseStatistics(struct mg_connection *conn);
    void dumpJobs(Connection *conn);
    std::shared_ptr<JobScheduler> jobScheduler() const { return mJobScheduler; }
private:
   void restoreFileIds();
    bool index(const String &arguments, const Path &pwd, const Path &projectRootOverride, bool escape);
    void onNewConnection(SocketServer *server);
    void setCurrentProject(const std::shared_ptr<Project> &project, unsigned int queryFlags = 0);
    void onUnload();
    void onNewMessage(const std::shared_ptr<Message> &message, Connection *conn);
    void clearProjects();
    void handleCompileMessage(const std::shared_ptr<CompileMessage> &message, Connection *conn);
    void handleIndexerMessage(const std::shared_ptr<IndexerMessage> &message, Connection *conn);
    void handleQueryMessage(const std::shared_ptr<QueryMessage> &message, Connection *conn);
    void handleErrorMessage(const std::shared_ptr<ErrorMessage> &message, Connection *conn);
    void handleLogOutputMessage(const std::shared_ptr<LogOutputMessage> &message, Connection *conn);
    void handleVisitFileMessage(const std::shared_ptr<VisitFileMessage> &message, Connection *conn);

    // Queries
    void sendDiagnostics(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void clearProjects(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void codeCompleteAt(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void cursorInfo(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void dependencies(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void dumpFile(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void findFile(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void findSymbols(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void fixIts(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void followLocation(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void hasFileManager(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void isIndexed(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void isIndexing(const std::shared_ptr<QueryMessage> &, Connection *conn);
    void jobCount(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void listSymbols(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void preprocessFile(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void project(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void referencesForLocation(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void referencesForName(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void reindex(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void reloadFileManager(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void reloadProjects(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void removeFile(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void removeProject(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void shutdown(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void sources(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void dumpCompletions(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void status(const std::shared_ptr<QueryMessage> &query, Connection *conn);
    void syncProject(const std::shared_ptr<QueryMessage> &qyery, Connection *conn);
    void suspend(const std::shared_ptr<QueryMessage> &query, Connection *conn);

    std::shared_ptr<Project> projectForQuery(const std::shared_ptr<QueryMessage> &queryMessage);
    std::shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    int reloadProjects();
    std::shared_ptr<Project> addProject(const Path &path);

    bool hasServer() const;
    void onHttpClientReadyRead(const SocketClient::SharedPtr &socket);
    void connectToServer();
    void startJobs();

    typedef Hash<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    bool mSuspended;
    SocketServer::SharedPtr mUnixServer;
    bool mVerbose;

    uint32_t mLastFileId;

    Timer mUnloadTimer;

    std::shared_ptr<JobScheduler> mJobScheduler;

    CompletionThread *mCompletionThread;
};

#endif
