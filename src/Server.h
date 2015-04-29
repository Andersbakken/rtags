/* This file is part of RTags (http://rtags.net).

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
#include "IndexMessage.h"
#include <rct/Connection.h>
#include <rct/FileSystemWatcher.h>
#include <rct/List.h>
#include <rct/Hash.h>
#include <rct/String.h>
#include <rct/Timer.h>
#include <rct/SocketServer.h>
#include <rct/Flags.h>

class CompletionThread;
class Connection;
class ErrorMessage;
class IndexDataMessage;
class QueryJob;
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
        Progress = 0x20000,
        Weverything = 0x40000,
        NoComments = 0x80000
    };
    struct Options {
        Options()
            : jobCount(0), headerErrorJobCount(0),
              rpVisitFileTimeout(0), rpIndexDataMessageTimeout(0), rpConnectTimeout(0),
              rpConnectAttempts(0), rpNiceValue(0), threadStackSize(0), maxCrashCount(0),
              completionCacheSize(0), testTimeout(60 * 1000 * 5),
              maxFileMapScopeCacheSize(512)
        {}
        Path socketFile, dataDir, argTransform;
        Flags<Option> options;
        int jobCount, headerErrorJobCount, rpVisitFileTimeout, rpIndexDataMessageTimeout,
            rpConnectTimeout, rpConnectAttempts, rpNiceValue, threadStackSize, maxCrashCount,
            completionCacheSize, testTimeout, maxFileMapScopeCacheSize;
        List<String> defaultArguments, excludeFilters;
        Set<String> blockedArguments;
        List<Source::Include> includePaths;
        List<Source::Define> defines;
        List<Path> tests;
        Set<Path> ignoredCompilers;
        List<std::pair<std::regex, Source::Language> > extraCompilers;

        inline bool flag(enum Option o) const { return 0 != (options & o); }
    };
    bool init(const Options &options);
    bool runTests();
    const Options &options() const { return mOptions; }
    bool suspended() const { return mSuspended; }
    std::shared_ptr<Project> project(const Path &path) const { return mProjects.value(path); }
    bool shouldIndex(const Source &source, const Path &project) const;
    void stopServers();
    int mongooseStatistics(struct mg_connection *conn);
    void dumpJobs(const std::shared_ptr<Connection> &conn);
    std::shared_ptr<JobScheduler> jobScheduler() const { return mJobScheduler; }
    const Set<uint32_t> &activeBuffers() const { return mActiveBuffers; }
    bool isActiveBuffer(uint32_t fileId) const { return mActiveBuffers.contains(fileId); }
    int exitCode() const { return mExitCode; }
private:
    String guessArguments(const String &args, const Path &pwd, const Path &projectRootOverride);
    bool saveFileIds();
    void restoreFileIds();
    bool index(const String &arguments,
               const Path &pwd,
               const Path &projectRootOverride,
               Flags<IndexMessage::Flag> = Flags<IndexMessage::Flag>());
    void onNewConnection(SocketServer *server);
    void setCurrentProject(const std::shared_ptr<Project> &project);
    void onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &conn);
    void clearProjects();
    void handleIndexMessage(const std::shared_ptr<IndexMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleQueryMessage(const std::shared_ptr<QueryMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleErrorMessage(const std::shared_ptr<ErrorMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleLogOutputMessage(const std::shared_ptr<LogOutputMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleVisitFileMessage(const std::shared_ptr<VisitFileMessage> &message, const std::shared_ptr<Connection> &conn);

    // Queries
    void sendDiagnostics(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void clearProjects(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void codeCompleteAt(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void cursorInfo(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dependencies(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dumpFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void generateTest(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void findFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void findSymbols(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void fixIts(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void followLocation(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void hasFileManager(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void includeFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void isIndexed(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void isIndexing(const std::shared_ptr<QueryMessage> &, const std::shared_ptr<Connection> &conn);
    void jobCount(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void listSymbols(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void preprocessFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void project(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void referencesForLocation(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void referencesForName(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void reindex(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void reloadFileManager(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void reloadProjects(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void removeFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void removeProject(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void sources(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dumpCompletions(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dumpCompilationDatabase(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void status(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void suspend(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void setBuffers(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void classHierarchy(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);

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

    int mExitCode;
    uint32_t mLastFileId;
    std::shared_ptr<JobScheduler> mJobScheduler;
    CompletionThread *mCompletionThread;
    Set<uint32_t> mActiveBuffers;
    Set<std::shared_ptr<Connection> > mConnections;

    Signal<std::function<void()> > mIndexDataMessageReceived;
    friend void saveFileIds();
};

RCT_FLAGS(Server::Option);

#endif
