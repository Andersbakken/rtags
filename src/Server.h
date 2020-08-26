/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef Server_h
#define Server_h

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <functional>
#include <memory>
#include <utility>

#include "IndexMessage.h"
#include "rct/Flags.h"
#include "rct/Hash.h"
#include "rct/List.h"
#include "rct/SocketServer.h"
#include "rct/String.h"
#include "rct/Thread.h"
#include "Source.h"
#include "RTags.h"
#include "rct/Path.h"
#include "rct/Set.h"
#include "rct/SignalSlot.h"

class IndexMessage;
class SocketServer;
struct SourceCache;
#ifdef OS_Darwin
#include <Availability.h>
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 101000
#include <launch.h>

#define RTAGS_HAS_LAUNCHD
#endif
#endif

class Match;
class CompletionThread;
class Connection;
class IndexDataMessage;
class QueryJob;
class LogOutputMessage;
class Message;
class OutputMessage;
class Project;
class QueryMessage;
class VisitFileMessage;
class JobScheduler;
class IndexParseData;

class Server
{
public:
    Server();
    ~Server();
    static Server *instance() { return sInstance; }
    enum Option {
        NoOptions = 0x0,
        ClearProjects = (1ull << 1),
        Wall = (1ull << 2),
        IgnorePrintfFixits = (1ull << 3),
        SpellChecking = (1ull << 4),
        AllowMultipleSources = (1ull << 5),
        NoStartupCurrentProject = (1ull << 6),
        WatchSystemPaths = (1ull << 7),
        NoFileManagerWatch = (1ull << 8),
        NoFileSystemWatch = (1ull << 9),
        NoNoUnknownWarningsOption = (1ull << 10),
        SuspendRPOnCrash = (1ull << 11),
        SeparateDebugAndRelease = (1ull << 12),
        AllowPedantic = (1ull << 13),
        StartSuspended = (1ull << 14),
        EnableCompilerManager = (1ull << 15),
        EnableNDEBUG = (1ull << 16),
        Progress = (1ull << 17),
        Weverything = (1ull << 18),
        NoComments = (1ull << 19),
        Launchd = (1ull << 20),
        RPLogToSyslog = (1ull << 21),
        CompletionsNoFilter = (1ull << 22),
        WatchSourcesOnly = (1ull << 23),
        NoFileLock = (1ull << 24),
        PCHEnabled = (1ull << 25),
        NoFileManager = (1ull << 26),
        ValidateFileMaps = (1ull << 27),
        CompletionLogs = (1ull << 28),
        AllowWErrorAndWFatalErrors = (1ull << 29),
        NoRealPath = (1ull << 30),
        Separate32BitAnd64Bit = (1ull << 31),
        SourceIgnoreIncludePathDifferencesInUsr = (1ull << 32),
        NoLibClangIncludePath = (1ull << 33),
        CompletionDiagnostics = (1ull << 34)
    };
    struct Options {
        Options()
            : jobCount(0), maxIncludeCompletionDepth(0),
              rpVisitFileTimeout(0), rpIndexDataMessageTimeout(0), rpConnectTimeout(0),
              rpConnectAttempts(0), rpNiceValue(0), maxCrashCount(0),
              completionCacheSize(0), testTimeout(60 * 1000 * 5),
              maxFileMapScopeCacheSize(512), pollTimer(0), maxSocketWriteBufferSize(0),
              daemonCount(0), tcpPort(0)
        {
        }

        Path socketFile, dataDir, argTransform, rp, sandboxRoot, tempDir;
        Flags<Option> options;
        size_t jobCount, maxIncludeCompletionDepth;
        int rpVisitFileTimeout, rpIndexDataMessageTimeout,
            rpConnectTimeout, rpConnectAttempts, rpNiceValue, maxCrashCount,
            completionCacheSize, testTimeout, maxFileMapScopeCacheSize, errorLimit,
            pollTimer, maxSocketWriteBufferSize, daemonCount;
        uint16_t tcpPort;
        List<String> defaultArguments, excludeFilters;
        Set<String> blockedArguments;
        List<Source::Include> includePaths;
        Set<Source::Define> defines;
        List<Path> tests;
        Set<Path> ignoredCompilers;
        Set<String> compilerWrappers;
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
    void dumpDaemons(const std::shared_ptr<Connection> &conn);
    std::shared_ptr<JobScheduler> jobScheduler() const { return mJobScheduler; }
    enum ActiveBufferType {
        Inactive,
        Active,
        Open
    };

    Set<uint32_t> activeBuffers(ActiveBufferType type) const
    {
        assert(type != Inactive);
        Set<uint32_t> ret;
        for (const auto &buffer : mActiveBuffers) {
            if (buffer.second == type) {
                ret.insert(buffer.first);
            }
        }
        return ret;
    }
    bool activeBuffersSet() const { return mActiveBuffersSet; }
    ActiveBufferType activeBufferType(uint32_t fileId) const { return mActiveBuffers.value(fileId, Inactive); }
    int exitCode() const { return mExitCode; }
    std::shared_ptr<Project> currentProject() const { return mCurrentProject.lock(); }
    Hash<Path, std::shared_ptr<Project> > projects() const { return mProjects; }
    void onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &conn);
    bool saveFileIds();
    bool loadCompileCommands(IndexParseData &data, const Path &compileCommands, const List<String> &environment, SourceCache *cache) const;
    bool parse(IndexParseData &data,
               String &&arguments,
               const Path &pwd,
               uint32_t compileCommandsFileId = 0,
               SourceCache *cache = nullptr) const;
    enum FileIdsFileFlag {
        None = 0x0,
        HasSandboxRoot = 0x1,
        HasNoRealPath = 0x2,
        HasRealPath = 0x4
    };

    void filterBlockedArguments(Source &source);
    void sourceFileModified(const std::shared_ptr<Project> &project, uint32_t fileId);
private:
    String guessArguments(const String &args, const Path &pwd, const Path &projectRootOverride) const;
    bool load();
    void onNewConnection(SocketServer *server);
    void setCurrentProject(const std::shared_ptr<Project> &project);
    enum ClearMode {
        Clear_All,
        Clear_KeepFileIds
    };
    void clearProjects(ClearMode mode);
    void handleIndexMessage(const std::shared_ptr<IndexMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleIndexDataMessage(const std::shared_ptr<IndexDataMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleQueryMessage(const std::shared_ptr<QueryMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleLogOutputMessage(const std::shared_ptr<LogOutputMessage> &message, const std::shared_ptr<Connection> &conn);
    void handleVisitFileMessage(const std::shared_ptr<VisitFileMessage> &message, const std::shared_ptr<Connection> &conn);

    // Queries
    void sendDiagnostics(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void clearProjects(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void deadFunctions(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void codeCompleteAt(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void symbolInfo(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void dependencies(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void includePath(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
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
    void lastIndexed(const std::shared_ptr<QueryMessage> &, const std::shared_ptr<Connection> &conn);
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
    void dumpCompileCommands(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void status(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void suspend(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void setBuffers(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void classHierarchy(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void debugLocations(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void tokens(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);
    void validate(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Connection> &conn);

    std::shared_ptr<Project> projectForQuery(const std::shared_ptr<QueryMessage> &queryMessage);
    std::shared_ptr<Project> projectForMatches(const List<Match> &matches);
    std::shared_ptr<Project> addProject(const Path &path);

    bool initServers();
    void removeSocketFile();
    void prepareCompletion(const std::shared_ptr<QueryMessage> &query, uint32_t fileId, const std::shared_ptr<Project> &project);

    typedef Hash<Path, std::shared_ptr<Project> > ProjectsMap;
    ProjectsMap mProjects;
    std::weak_ptr<Project> mCurrentProject;

    static Server *sInstance;
    Options mOptions;
    bool mSuspended;
    std::shared_ptr<SocketServer> mUnixServer, mTcpServer;
    List<String> mEnvironment;

    int mPollTimer, mExitCode;
    uint32_t mLastFileId;
    std::shared_ptr<JobScheduler> mJobScheduler;
    CompletionThread *mCompletionThread;
    bool mActiveBuffersSet;
    Hash<uint32_t, ActiveBufferType> mActiveBuffers;
    Set<std::shared_ptr<Connection> > mConnections;

    Signal<std::function<void()> > mIndexDataMessageReceived;
    size_t mDefaultJobCount { 0 };
    List<size_t> mJobCountStack;
};
RCT_FLAGS(Server::Option);
RCT_FLAGS(Server::FileIdsFileFlag);

#endif
