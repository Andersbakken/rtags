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

#include "IndexMessage.h"
#include "rct/Flags.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/SocketServer.h"
#include "rct/String.h"
#include "rct/Thread.h"
#include "Source.h"
#ifdef OS_Darwin
#include <Availability.h>
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
#include <launch.h>
#define RTAGS_HAS_LAUNCHD
#endif
#endif

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
        NoOptions = 0x000000,
        ClearProjects = 0x000001,
        Wall = 0x000002,
        IgnorePrintfFixits = 0x000004,
        NoUnlimitedErrors = 0x000008,
        SpellChecking = 0x000010,
        DisallowMultipleSources = 0x000020,
        NoStartupCurrentProject = 0x000040,
        WatchSystemPaths = 0x000080,
        NoFileManagerWatch = 0x000100,
        NoFileSystemWatch = 0x000200,
        NoNoUnknownWarningsOption = 0x000400,
        SuspendRPOnCrash = 0x000800,
        SeparateDebugAndRelease = 0x001000,
        AllowPedantic = 0x002000,
        StartSuspended = 0x004000,
        EnableCompilerManager = 0x008000,
        EnableNDEBUG = 0x010000,
        Progress = 0x020000,
        Weverything = 0x040000,
        NoComments = 0x080000,
        Launchd = 0x0100000,     /* Only valid for Darwin... but you're not out of bits yet. */
        RPLogToSyslog = 0x0200000,
        CompletionsNoFilter = 0x0400000,
        WatchSourcesOnly = 0x0800000,
        NoFileLock = 0x1000000,
        PCHEnabled = 0x2000000,
        NoFileManager = 0x4000000,
        ValidateFileMaps = 0x8000000
    };
    struct Options {
        Options()
            : jobCount(0), headerErrorJobCount(0), maxIncludeCompletionDepth(0),
              rpVisitFileTimeout(0), rpIndexDataMessageTimeout(0), rpConnectTimeout(0),
              rpConnectAttempts(0), rpNiceValue(0), threadStackSize(0), maxCrashCount(0),
              completionCacheSize(0), testTimeout(60 * 1000 * 5),
              maxFileMapScopeCacheSize(512), tcpPort(0)
        {
        }

        Path socketFile, dataDir, argTransform, rp, sandboxRoot;
        Flags<Option> options;
        size_t jobCount, headerErrorJobCount, maxIncludeCompletionDepth;
        int rpVisitFileTimeout, rpIndexDataMessageTimeout,
            rpConnectTimeout, rpConnectAttempts, rpNiceValue, threadStackSize, maxCrashCount,
            completionCacheSize, testTimeout, maxFileMapScopeCacheSize;
        uint16_t tcpPort;
        List<String> defaultArguments, excludeFilters;
        Set<String> blockedArguments;
        List<Source::Include> includePaths;
        List<Source::Define> defines;
        List<Path> tests;
        Set<Path> ignoredCompilers;
        List<std::regex> extraCompilers;
        List<String> debugLocations;
    };
    bool init(const Options &options);
    bool runTests();
    const Options &options() const { return mOptions; }
    bool suspended() const { return mSuspended; }
    std::shared_ptr<Project> project(const Path &path) const { return mProjects.value(path); }
    bool shouldIndex(const Source &source, const Path &project) const;
    void stopServers();
    void dumpJobs(const std::shared_ptr<Connection> &conn);
    std::shared_ptr<JobScheduler> jobScheduler() const { return mJobScheduler; }
    const Set<uint32_t> &activeBuffers() const { return mActiveBuffers; }
    bool isActiveBuffer(uint32_t fileId) const { return mActiveBuffers.contains(fileId); }
    int exitCode() const { return mExitCode; }
    std::shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    void onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &conn);
    bool saveFileIds();
    bool index(const String &arguments,
               const Path &pwd,
               const List<Path> &pathEnvironment,
               const Path &projectRootOverride,
               Flags<IndexMessage::Flag> flags = Flags<IndexMessage::Flag>(),
               std::shared_ptr<Project> *projectPtr = 0,
               Set<uint64_t> *indexed = 0);
    enum FileIdsFileFlag {
        None = 0x0,
        HasSandboxRoot = 0x1
    };
private:
    String guessArguments(const String &args, const Path &pwd, const Path &projectRootOverride);
    bool load();
    void onNewConnection(SocketServer *server);
    void setCurrentProject(const std::shared_ptr<Project> &project);
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
    void startClangThread(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dumpFileMaps(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void diagnose(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
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
    void removeFile(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void removeProject(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void sources(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dumpCompletions(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dumpCompilationDatabase(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void status(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void suspend(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void setBuffers(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void classHierarchy(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void debugLocations(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void tokens(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);

    std::shared_ptr<Project> projectForQuery(const std::shared_ptr<QueryMessage> &queryMessage);
    std::shared_ptr<Project> addProject(const Path &path);

    bool initServers();
    void removeSocketFile();

    typedef Hash<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    bool mSuspended;
    SocketServer::SharedPtr mUnixServer, mTcpServer;
    List<Path> mPathEnvironment;

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
RCT_FLAGS(Server::FileIdsFileFlag);

#endif
