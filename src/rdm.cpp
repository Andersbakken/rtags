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

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <algorithm>
#include <functional>
#include <initializer_list>
#include <memory>
#include <utility>
#ifdef OS_Darwin
#include <sys/resource.h>
#endif

#include "rct/EventLoop.h"
#include "rct/Log.h"
#include "rct/Process.h"
#include "rct/rct-config.h"
#include "rct/Rct.h"
#include "rct/ThreadPool.h"
#include "RTags.h"
#include "CommandLineParser.h"
#include "Server.h"
#include "Source.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Path.h"
#include "rct/Set.h"
#include "rct/String.h"
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#if !defined(HAVE_FSEVENTS) && defined(HAVE_KQUEUE)
#define FILEMANAGER_OPT_IN
#endif

char crashDumpTempFilePath[PATH_MAX];
char crashDumpFilePath[PATH_MAX - 4];
FILE *crashDumpFile = nullptr;
static void signalHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);

#ifdef HAVE_BACKTRACE
    enum { SIZE = 1024 };
    void *stack[SIZE];

    const int frameCount = backtrace(stack, sizeof(stack) / sizeof(void*));
    if (frameCount <= 0) {
        fprintf(stderr, "Couldn't get stack trace\n");
        if (crashDumpFile)
            fprintf(crashDumpFile, "Caught signal %d\nCouldn't get stack trace\n", signal);
    } else {
        backtrace_symbols_fd(stack, frameCount, fileno(stderr));
        if (crashDumpFile) {
            backtrace_symbols_fd(stack, frameCount, fileno(crashDumpFile));
            fprintf(crashDumpFile, "Caught signal %d\n", signal);
        }
    }
#endif
    fflush(stderr);

    if (crashDumpFile) {
        fclose(crashDumpFile);
        rename(crashDumpTempFilePath, crashDumpFilePath);
        crashDumpFile = nullptr;
    }
    if (Server *server = Server::instance())
        server->stopServers();
    _exit(1);
}

static const char *DEFAULT_EXCLUDEFILTER = "*/CMakeFiles/*;*/cmake*/Modules/*;*/conftest.c*;/tmp/*;/private/tmp/*;/private/var/*";
static const char *DEFAULT_BLOCKED_ARGUMENTS = "-save-temps;-save-temps=";
static const char *DEFAULT_COMPILER_WRAPPERS = "ccache";
#ifdef NDEBUG
const char *DEFAULT_SUSPEND_RP = "off";
#else
const char *DEFAULT_SUSPEND_RP = "on";
#endif

enum {
    DEFAULT_RP_VISITFILE_TIMEOUT = 60000,
    DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE = 500,
    DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT = 60000,
    DEFAULT_RP_CONNECT_TIMEOUT = 0, // won't time out
    DEFAULT_RP_CONNECT_ATTEMPTS = 3,
    DEFAULT_COMPLETION_CACHE_SIZE = 10,
    DEFAULT_ERROR_LIMIT = 50,
    DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH = 3,
    DEFAULT_MAX_CRASH_COUNT = 5
};

static inline Path defaultRP()
{
    static Path rp;
    if (rp.isEmpty()) {
        rp = Rct::executablePath().parentDir() + "rp";
        if (!rp.isFile()) {
            rp = Rct::executablePath();
            rp.resolve();
            rp = rp.parentDir() + "rp";
            if (!rp.isFile()) // should be in $PATH
                rp = "rp";
        }
    }
    return rp;
}

class RemoveCrashDump
{
public:
    ~RemoveCrashDump()
    {
        if (crashDumpFile) {
            fclose(crashDumpFile);
            crashDumpFile = nullptr;
            unlink(crashDumpTempFilePath);
        }
    }
};

enum OptionType {
    None = 0,
    Help,
    Version,
    IncludePath,
    Isystem,
    Define,
    DefaultArgument,
    LogFile,
    CrashDumpFile,
    SetEnv,
    NoWall,
    Weverything,
    Verbose,
    JobCount,
    TempDir,
    Test,
    TestTimeout,
    CleanSlate,
    DisableSigHandler,
    Silent,
    ExcludeFilter,
    SocketFile,
    DataDir,
    IgnorePrintfFixits,
    ErrorLimit,
    BlockArgument,
    NoSpellChecking,
    LargeByValueCopy,
    AllowMultipleSources,
    NoStartupProject,
    NoLibClangIncludePath,
    NoNoUnknownWarningsOption,
    IgnoreCompiler,
    CompilerWrappers,
    WatchSystemPaths,
    RPVisitFileTimeout,
    RPIndexerMessageTimeout,
    RPConnectTimeout,
    RPConnectAttempts,
    RPNiceValue,
    SuspendRPOnCrash,
    RPLogToSyslog,
    RPDaemon,
    StartSuspended,
    SeparateDebugAndRelease,
    Separate32BitAnd64Bit,
    SourceIgnoreIncludePathDifferencesInUsr,
    MaxCrashCount,
    MaxSocketWriteBufferSize,
    CompletionCacheSize,
    CompletionDiagnostics,
    CompletionNoFilter,
    CompletionLogs,
    MaxIncludeCompletionDepth,
    AllowWpedantic,
    AllowWErrorAndWFatalErrors,
    EnableCompilerManager,
    EnableNDEBUG,
    Progress,
    MaxFileMapCacheSize,
#ifdef FILEMANAGER_OPT_IN
    FileManagerWatch,
#else
    NoFileManagerWatch,
#endif
    NoFileManager,
    NoFileLock,
    PchEnabled,
    NoFilesystemWatcher,
    ArgTransform,
    NoComments,
#ifdef RTAGS_HAS_LAUNCHD
    Launchd,
#endif
    InactivityTimeout,
    Daemon,
    LogFileLogLevel,
    WatchSourcesOnly,
    DebugLocations,
    ValidateFileMaps,
    TcpPort,
    RPPath,
    LogTimestamp,
    LogFlushOption,
    SandboxRoot,
    PollTimer,
    NoRealPath,
    Noop
};

int main(int argc, char** argv)
{
    RemoveCrashDump removeCrashDump;
#ifdef OS_Darwin
    struct rlimit rlp;
    if (getrlimit(RLIMIT_NOFILE, &rlp) == 0) {
        if (rlp.rlim_cur < 1000) {
            rlp.rlim_cur = 1000;
            setrlimit(RLIMIT_NOFILE, &rlp);
        }
    }
#endif

    Rct::findExecutablePath(*argv);

    bool daemon = false;
    Server::Options serverOpts;
    const char * runtimeDir = getenv("XDG_RUNTIME_DIR");
    if (runtimeDir == nullptr) {
        serverOpts.socketFile = String::format<128>("%s.rdm", Path::home().constData());
    } else {
        serverOpts.socketFile = String::format<1024>("%s/rdm.socket", runtimeDir);
    }
    const char *tempDir = nullptr;
    for (const char *tmp : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" }) {
        if ((tempDir = getenv(tmp))) {
            break;
        }
    }
    if (!tempDir)
        tempDir = "/tmp";

    serverOpts.tempDir = tempDir;
    serverOpts.jobCount = std::max(2, ThreadPool::idealThreadCount());
    serverOpts.rpVisitFileTimeout = DEFAULT_RP_VISITFILE_TIMEOUT;
    serverOpts.rpIndexDataMessageTimeout = DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT;
    serverOpts.rpConnectTimeout = DEFAULT_RP_CONNECT_TIMEOUT;
    serverOpts.rpConnectAttempts = DEFAULT_RP_CONNECT_ATTEMPTS;
    serverOpts.maxFileMapScopeCacheSize = DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE;
    serverOpts.errorLimit = DEFAULT_ERROR_LIMIT;
    serverOpts.rpNiceValue = INT_MIN;
    serverOpts.options = Server::Wall|Server::SpellChecking|Server::CompletionDiagnostics;
    serverOpts.maxCrashCount = DEFAULT_MAX_CRASH_COUNT;
    serverOpts.completionCacheSize = DEFAULT_COMPLETION_CACHE_SIZE;
    serverOpts.maxIncludeCompletionDepth = DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH;
    serverOpts.rp = defaultRP();
    serverOpts.blockedArguments = String::split(DEFAULT_BLOCKED_ARGUMENTS, ';').toSet();
    strcpy(crashDumpFilePath, "crash.dump");
#ifdef FILEMANAGER_OPT_IN
    serverOpts.options |= Server::NoFileManagerWatch;
#endif
    // #ifndef NDEBUG
    //     serverOpts.options |= Server::SuspendRPOnCrash;
    // #endif
    serverOpts.dataDir = String::format<128>("%s.rtags", Path::home().constData());
    if (!serverOpts.dataDir.exists()) {
         const char * dataDir = getenv("XDG_CACHE_HOME");
         serverOpts.dataDir = dataDir ? dataDir : Path::home() + ".cache";
         serverOpts.dataDir += "/rtags/";
         serverOpts.dataDir.mkdir(Path::Recursive);
    }
    Path logFile;
    Flags<LogFlag> logFlags = DontRotate|LogStderr;
    LogLevel logLevel(LogLevel::Error);
    LogLevel logFileLogLevel(LogLevel::Error);
    bool sigHandler = true;
    assert(Path::home().endsWith('/'));
    int inactivityTimeout = 0;

    const std::initializer_list<CommandLineParser::Option<OptionType> > opts = {
        { None, nullptr, 0, CommandLineParser::NoValue, "Options:" },
        { Help, "help", 'h', CommandLineParser::NoValue, "Display this page." },
        { Version, "version", 0, CommandLineParser::NoValue, "Display version." },
        { IncludePath, "include-path", 'I', CommandLineParser::Required, "Add additional include path to clang." },
        { NoLibClangIncludePath, "no-libclang-include-path", 0, CommandLineParser::NoValue, "Don't use the include path from libclang." },
        { Isystem, "isystem", 's', CommandLineParser::Required, "Add additional system include path to clang." },
        { Define, "define", 'D', CommandLineParser::Required, "Add additional define directive to clang." },
        { DefaultArgument, "default-argument", 0, CommandLineParser::Required, "Add additional argument to clang." },
        { LogFile, "log-file", 'L', CommandLineParser::Required, "Log to this file." },
        { CrashDumpFile, "crash-dump-file", 0, CommandLineParser::Required, "File to dump crash log to (default is <datadir>/crash.dump)." },
        { SetEnv, "setenv", 'e', CommandLineParser::Required, "Set this environment variable (--setenv \"foobar=1\")." },
        { NoWall, "no-Wall", 'W', CommandLineParser::NoValue, "Don't use -Wall." },
        { Weverything, "Weverything", 'u', CommandLineParser::NoValue, "Use -Weverything." },
        { Verbose, "verbose", 'v', CommandLineParser::NoValue, "Change verbosity, multiple -v's are allowed." },
        { JobCount, "job-count", 'j', CommandLineParser::Required, String::format("Spawn this many concurrent processes for indexing (default to the number of available processing units or 2 otherwise).") },
        { Test, "test", 't', CommandLineParser::Required, "Run this test." },
        { TempDir, "tempdir", 0, CommandLineParser::Required, "Use this directory for temporary files. Clang generates a lot of these and rtags will periodically clean out this directory (default is $TMPDIR/rtags/)" },
        { TestTimeout, "test-timeout", 'z', CommandLineParser::Required, "Timeout for test to complete." },
        { CleanSlate, "clean-slate", 'C', CommandLineParser::NoValue, "Clear out all data." },
        { DisableSigHandler, "disable-sighandler", 'x', CommandLineParser::NoValue, "Disable signal handler to dump stack for crashes." },
        { Silent, "silent", 'S', CommandLineParser::NoValue, "No logging to stdout/stderr." },
        { ExcludeFilter, "exclude-filter", 'X', CommandLineParser::Required, String::format("Files to exclude from rdm (default \"%s\".)", DEFAULT_EXCLUDEFILTER) },
        { SocketFile, "socket-file", 'n', CommandLineParser::Required, "Use this file for the server socket (default is XDG_RUNTIME_DIR/rdm.socket else ~/.rdm)." },
        { DataDir, "data-dir", 'd', CommandLineParser::Required, "Use this directory to store persistent data (default $XDG_CACHE_HOME/rtags otherwise ~/.cache/rtags)." },
        { IgnorePrintfFixits, "ignore-printf-fixits", 'F', CommandLineParser::NoValue, "Disregard any clang fixit that looks like it's trying to fix format for printf and friends." },
        { ErrorLimit, "error-limit", 'f', CommandLineParser::Required, String::format("Set error limit to argument (-ferror-limit={arg} (default %d).", DEFAULT_ERROR_LIMIT) },
        { BlockArgument, "block-argument", 'G', CommandLineParser::Required, "Block this argument from being passed to clang. E.g. rdm --block-argument -fno-inline" },
        { NoSpellChecking, "no-spell-checking", 'l', CommandLineParser::NoValue, "Don't pass -fspell-checking." },
        { LargeByValueCopy, "large-by-value-copy", 'r', CommandLineParser::Required, "Use -Wlarge-by-value-copy=[arg] when invoking clang." },
        { AllowMultipleSources, "allow-multiple-sources", 'm', CommandLineParser::NoValue, "Don't merge source files added with -c." },
        { NoStartupProject, "no-startup-project", 'o', CommandLineParser::NoValue, "Don't restore the last current project on startup." },
        { NoNoUnknownWarningsOption, "no-no-unknown-warnings-option", 'Y', CommandLineParser::NoValue, "Don't pass -Wno-unknown-warning-option." },
        { IgnoreCompiler, "ignore-compiler", 'b', CommandLineParser::Required, "Ignore this compiler." },
        { CompilerWrappers, "compiler-wrappers", 0, CommandLineParser::Required, String::format("Consider these filenames compiler wrappers (split on ;), default \"%s\".", DEFAULT_COMPILER_WRAPPERS) },
        { WatchSystemPaths, "watch-system-paths", 'w', CommandLineParser::NoValue, "Watch system paths for changes." },
        { RPVisitFileTimeout, "rp-visit-file-timeout", 'Z', CommandLineParser::Required, String::format("Timeout for rp visitfile commands in ms (0 means no timeout) (default %d).", DEFAULT_RP_VISITFILE_TIMEOUT) },
        { RPIndexerMessageTimeout, "rp-indexer-message-timeout", 'T', CommandLineParser::Required, String::format("Timeout for rp indexer-message in ms (0 means no timeout) (default %d).", DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT) },
        { RPConnectTimeout, "rp-connect-timeout", 'O', CommandLineParser::Required, String::format("Timeout for connection from rp to rdm in ms (0 means no timeout) (default %d).", DEFAULT_RP_CONNECT_TIMEOUT) },
        { RPConnectAttempts, "rp-connect-attempts", 0, CommandLineParser::Required, String::format("Number of times rp attempts to connect to rdm before giving up. (default %d).", DEFAULT_RP_CONNECT_ATTEMPTS) },
        { RPNiceValue, "rp-nice-value", 'a', CommandLineParser::Required, "Nice value to use for rp (nice(2)) (default is no nicing)." },
        { SuspendRPOnCrash, "suspend-rp-on-crash", 'q', CommandLineParser::NoValue, String::format("Suspend rp in SIGSEGV handler (default %s).", DEFAULT_SUSPEND_RP) },
        { RPLogToSyslog, "rp-log-to-syslog", 0, CommandLineParser::NoValue, "Make rp log to syslog." },
        { StartSuspended, "start-suspended", 'Q', CommandLineParser::NoValue, "Start out suspended (no reindexing enabled)." },
        { SeparateDebugAndRelease, "separate-debug-and-release", 'E', CommandLineParser::NoValue, "Normally rdm doesn't consider release and debug as different builds. Pass this if you want it to." },
        { Separate32BitAnd64Bit, "separate-32-bit-and-64-bit", 0, CommandLineParser::NoValue, "Normally rdm doesn't consider -m32 and -m64 as different builds. Pass this if you want it to." },
        { SourceIgnoreIncludePathDifferencesInUsr, "ignore-include-path-differences-in-usr", 0, CommandLineParser::NoValue, "Don't consider sources that only differ in includepaths within /usr (not including /usr/home/) as different builds." },
        { MaxCrashCount, "max-crash-count", 'K', CommandLineParser::Required, String::format("Max number of crashes before giving up a sourcefile (default %d).", DEFAULT_MAX_CRASH_COUNT) },
        { MaxSocketWriteBufferSize, "max-socket-write-buffer-size", 0, CommandLineParser::Required, "Max number of bytes buffered after EAGAIN." },
        { CompletionCacheSize, "completion-cache-size", 'i', CommandLineParser::Required, String::format("Number of translation units to cache (default %d).", DEFAULT_COMPLETION_CACHE_SIZE) },
        { CompletionNoFilter, "completion-no-filter", 0, CommandLineParser::NoValue, "Don't filter private members and destructors from completions." },
        { CompletionLogs, "completion-logs", 0, CommandLineParser::NoValue, "Log more info about completions." },
        { CompletionDiagnostics, "completion-diagnostics", 0, CommandLineParser::Optional, "Send diagnostics from completion thread." },
        { RPDaemon, "rp-daemon", 0, CommandLineParser::Required, "Keep this many rp daemons alive and cache the last tu. Default to 1" },
        { MaxIncludeCompletionDepth, "max-include-completion-depth", 0, CommandLineParser::Required, String::format("Max recursion depth for header completion (default %d).", DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH) },
        { AllowWpedantic, "allow-Wpedantic", 'P', CommandLineParser::NoValue, "Don't strip out -Wpedantic. This can cause problems in certain projects." },
        { AllowWErrorAndWFatalErrors, "allow-Werror", 0, CommandLineParser::NoValue, "Don't strip out -Werror and -Wfatal-errors. By default these are stripped out. " },
        { EnableCompilerManager, "enable-compiler-manager", 'R', CommandLineParser::NoValue, "Query compilers for their actual include paths instead of letting clang use its own." },
        { EnableNDEBUG, "enable-NDEBUG", 'g', CommandLineParser::NoValue, "Don't remove -DNDEBUG from compile lines." },
        { Progress, "progress", 'p', CommandLineParser::NoValue, "Report compilation progress in diagnostics output." },
        { MaxFileMapCacheSize, "max-file-map-cache-size", 'y', CommandLineParser::Required, String::format("Max files to cache per query (Should not exceed maximum number of open file descriptors allowed per process) (default %d).", DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE) },
#ifdef FILEMANAGER_OPT_IN
        { FileManagerWatch, "filemanager-watch", 'M', CommandLineParser::NoValue, "Use a file system watcher for filemanager." },
#else
        { NoFileManagerWatch, "no-filemanager-watch", 'M', CommandLineParser::NoValue, "Don't use a file system watcher for filemanager." },
#endif
        { NoFileManager, "no-filemanager", 0, CommandLineParser::NoValue, "Don't scan project directory for files. (rc -P won't work)." },
        { NoFileLock, "no-file-lock", 0, CommandLineParser::NoValue, "Disable file locking. Not entirely safe but might improve performance on certain systems." },
        { PchEnabled, "pch-enabled", 0, CommandLineParser::NoValue, "Enable PCH (experimental)." },
        { NoFilesystemWatcher, "no-filesystem-watcher", 'B', CommandLineParser::NoValue, "Disable file system watching altogether. Reindexing has to be triggered manually." },
        { ArgTransform, "arg-transform", 'V', CommandLineParser::Required, "Use arg to transform arguments. [arg] should be executable with (execv(3))." },
        { NoComments, "no-comments", 0, CommandLineParser::NoValue, "Don't parse/store doxygen comments." },
#ifdef RTAGS_HAS_LAUNCHD
        { Launchd, "launchd", 0, CommandLineParser::NoValue, "Run as a launchd job (use launchd API to retrieve socket opened by launchd on rdm's behalf)." },
#endif
        { InactivityTimeout, "inactivity-timeout", 0, CommandLineParser::Required, "Time in seconds after which rdm will quit if there's been no activity (N.B., once rdm has quit, something will need to re-run it!)." },
        { Daemon, "daemon", 0, CommandLineParser::NoValue, "Run as daemon (detach from terminal)." },
        { LogFileLogLevel, "log-file-log-level", 0, CommandLineParser::Required, "Log level for log file (default is error), options are: error, warning, debug or verbose-debug." },
        { WatchSourcesOnly, "watch-sources-only", 0, CommandLineParser::NoValue, "Only watch source files (not dependencies)." },
        { DebugLocations, "debug-locations", 0, CommandLineParser::Required, "Set debug locations." },
        { ValidateFileMaps, "validate-file-maps", 0, CommandLineParser::NoValue, "Spend some time validating project data on startup." },
        { TcpPort, "tcp-port", 0, CommandLineParser::Required, "Listen on this tcp socket (default none)." },
        { RPPath, "rp-path", 0, CommandLineParser::Required, String::format<256>("Path to rp (default %s).", defaultRP().constData()) },
        { LogTimestamp, "log-timestamp", 0, CommandLineParser::NoValue, "Add timestamp to logs." },
        { LogFlushOption, "log-flush", 0, CommandLineParser::NoValue, "Flush stderr/stdout after each log." },
        { SandboxRoot, "sandbox-root",  0, CommandLineParser::Required, "Create index using relative paths by stripping dir (enables copying of tag index db files without need to reindex)." },
        { PollTimer, "poll-timer", 0, CommandLineParser::Required, "Poll the database of the current project every <arg> seconds. " },
        { NoRealPath, "no-realpath", 0, CommandLineParser::NoValue, "Don't use realpath(3) for files" },
        { Noop, "config", 'c', CommandLineParser::Required, "Use this file (instead of ~/.rdmrc)." },
        { Noop, "no-rc", 'N', CommandLineParser::NoValue, "Don't load any rc files." }
    };

    std::function<CommandLineParser::ParseStatus(OptionType type, String &&value, size_t &idx, const List<String> &args)> cb;
    cb = [&](OptionType type, String &&value, size_t &, const List<String> &) -> CommandLineParser::ParseStatus {
        switch (type) {
        case None:
        case Noop:
        break;
        case Help: {
            CommandLineParser::help(stdout, Rct::executablePath().fileName(), opts);
            return { String(), CommandLineParser::Parse_Ok }; }
        case Version: {
            fprintf(stdout, "%s\n", RTags::versionString().constData());
            return { String(), CommandLineParser::Parse_Ok }; }
        case IncludePath: {
            serverOpts.includePaths.append(Source::Include(Source::Include::Type_Include, Path::resolved(value)));
            break; }
        case Isystem: {
            serverOpts.includePaths.append(Source::Include(Source::Include::Type_System, Path::resolved(value)));
            break; }
        case Define: {
            const size_t eq = value.indexOf('=');
            Source::Define def;
            if (eq == String::npos) {
                def.define = std::move(value);
            } else {
                def.define = value.left(eq);
                def.value = value.mid(eq + 1);
            }
            serverOpts.defines.insert(def);
            break; }
        case DefaultArgument: {
            serverOpts.defaultArguments.append(std::move(value));
            break; }
        case LogFile: {
            logFile = std::move(value);
            logFile.resolve();
            logLevel = LogLevel::None;
            break; }
        case CrashDumpFile: {
            strncpy(crashDumpFilePath, value.constData(), sizeof(crashDumpFilePath) - 1);
            break; }
        case SetEnv: {
            putenv(&value[0]);
            break; }
        case NoWall: {
            serverOpts.options &= ~Server::Wall;
            break; }
        case Weverything: {
            serverOpts.options |= Server::Weverything;
            break; }
        case Verbose: {
            if (logLevel != LogLevel::None)
                ++logLevel;
            break; }
        case JobCount: {
            bool ok;
            serverOpts.jobCount = String(value).toULong(&ok);
            if (!ok) {
                return { String::format<1024>("Can't parse argument to -j %s. -j must be a positive integer.\n", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case Test: {
            Path test(value);
            if (!test.resolve() || !test.isFile()) {
                return { String::format<1024>("%s doesn't seem to be a file", value.constData()), CommandLineParser::Parse_Error };
            }
            serverOpts.tests += test;
            break; }
        case TempDir: {
            serverOpts.tempDir = value;
            break; }
        case TestTimeout: {
            serverOpts.testTimeout = atoi(value.constData());
            if (serverOpts.testTimeout <= 0) {
                return { String::format<1024>("Invalid argument to -z %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case PollTimer: {
            serverOpts.pollTimer = atoi(value.constData());
            if (serverOpts.pollTimer < 0) {
                return { String::format<1024>("Invalid argument to --poll-timer %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case CleanSlate: {
            serverOpts.options |= Server::ClearProjects;
            break; }
        case DisableSigHandler: {
            sigHandler = false;
            break; }
        case Silent: {
            logLevel = LogLevel::None;
            break; }
        case ExcludeFilter: {
            serverOpts.excludeFilters += String(value).split(';');
            break; }
        case SocketFile: {
            serverOpts.socketFile = std::move(value);
            break; }
        case DataDir: {
            serverOpts.dataDir = String::format<128>("%s", Path::resolved(value).constData());
            break; }
        case IgnorePrintfFixits: {
            serverOpts.options |= Server::IgnorePrintfFixits;
            break; }
        case ErrorLimit: {
            bool ok;
            serverOpts.errorLimit = String(value).toULong(&ok);
            if (!ok) {
                return { String::format<1024>("Can't parse argument to --error-limit %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case BlockArgument: {
            serverOpts.blockedArguments << value;
            break; }
        case NoSpellChecking: {
            serverOpts.options &= ~Server::SpellChecking;
            break; }
        case LargeByValueCopy: {
            int large = atoi(value.constData());
            if (large <= 0) {
                return { String::format<1024>("Can't parse argument to -r %s", value.constData()), CommandLineParser::Parse_Error };
            }
            serverOpts.defaultArguments.append("-Wlarge-by-value-copy=" + String(value)); // ### not quite working
            break; }
        case AllowMultipleSources: {
            serverOpts.options |= Server::AllowMultipleSources;
            break; }
        case NoStartupProject: {
            serverOpts.options |= Server::NoStartupCurrentProject;
            break; }
        case NoLibClangIncludePath: {
            serverOpts.options |= Server::NoLibClangIncludePath;
            break; }
        case NoNoUnknownWarningsOption: {
            serverOpts.options |= Server::NoNoUnknownWarningsOption;
            break; }
        case IgnoreCompiler: {
            serverOpts.ignoredCompilers.insert(Path::resolved(value));
            break; }
        case CompilerWrappers: {
            serverOpts.compilerWrappers = String::split(value, ";", String::SkipEmpty).toSet();
            break; }
        case WatchSystemPaths: {
            serverOpts.options |= Server::WatchSystemPaths;
            break; }
        case RPVisitFileTimeout: {
            serverOpts.rpVisitFileTimeout = atoi(value.constData());
            if (serverOpts.rpVisitFileTimeout < 0) {
                return { String::format<1024>("Invalid argument to -Z %s", value.constData()), CommandLineParser::Parse_Error };
            }
            if (!serverOpts.rpVisitFileTimeout)
                serverOpts.rpVisitFileTimeout = -1;
            break; }
        case RPIndexerMessageTimeout: {
            serverOpts.rpIndexDataMessageTimeout = atoi(value.constData());
            if (serverOpts.rpIndexDataMessageTimeout <= 0) {
                return { String::format<1024>("Can't parse argument to -T %s.", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case RPConnectTimeout: {
            serverOpts.rpConnectTimeout = atoi(value.constData());
            if (serverOpts.rpConnectTimeout < 0) {
                return { String::format<1024>("Invalid argument to -O %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case RPConnectAttempts: {
            serverOpts.rpConnectAttempts = atoi(value.constData());
            if (serverOpts.rpConnectAttempts <= 0) {
                return { String::format<1024>("Invalid argument to --rp-connect-attempts %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case RPNiceValue: {
            bool ok;
            serverOpts.rpNiceValue = value.toLong(&ok);
            if (!ok) {
                return { String::format<1024>("Can't parse argument to -a %s.", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case SuspendRPOnCrash: {
            serverOpts.options |= Server::SuspendRPOnCrash;
            break; }
        case RPLogToSyslog: {
            serverOpts.options |= Server::RPLogToSyslog;
            break; }
        case RPDaemon: {
            serverOpts.daemonCount = atoi(value.constData());
            if (!serverOpts.daemonCount && value != "0")
                serverOpts.daemonCount = -1;
            if (serverOpts.daemonCount < 0) {
                return { String::format<1024>("Invalid argument to --rp-daemon %s", value.constData()), CommandLineParser::Parse_Error };
            }

            break; }
        case StartSuspended: {
            serverOpts.options |= Server::StartSuspended;
            break; }
        case SeparateDebugAndRelease: {
            serverOpts.options |= Server::SeparateDebugAndRelease;
            break; }
        case Separate32BitAnd64Bit: {
            serverOpts.options |= Server::Separate32BitAnd64Bit;
            break; }
        case SourceIgnoreIncludePathDifferencesInUsr: {
            serverOpts.options |= Server::SourceIgnoreIncludePathDifferencesInUsr;
            break; }
        case MaxCrashCount: {
            serverOpts.maxCrashCount = atoi(value.constData());
            if (serverOpts.maxCrashCount <= 0) {
                return { String::format<1024>("Invalid argument to -K %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case MaxSocketWriteBufferSize: {
            char *end;
            serverOpts.maxSocketWriteBufferSize = strtoul(value.constData(), &end, 10);
            if (*end || serverOpts.maxSocketWriteBufferSize < 0) {
                return { String::format<1024>("Invalid argument to --max-socket-write-buffer-size %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case CompletionCacheSize: {
            serverOpts.completionCacheSize = atoi(value.constData());
            if (serverOpts.completionCacheSize <= 0) {
                return { String::format<1024>("Invalid argument to -i %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case CompletionDiagnostics: {
            if (value == "off" || value == "false" || value == "0") {
                serverOpts.options &= ~Server::CompletionDiagnostics;
            } else {
                serverOpts.options |= Server::CompletionDiagnostics;
            }
            break; }
        case CompletionNoFilter: {
            serverOpts.options |= Server::CompletionsNoFilter;
            break; }
        case CompletionLogs: {
            serverOpts.options |= Server::CompletionLogs;
            break; }
        case MaxIncludeCompletionDepth: {
            serverOpts.maxIncludeCompletionDepth = strtoul(value.constData(), nullptr, 10);
            break; }
        case AllowWpedantic: {
            serverOpts.options |= Server::AllowPedantic;
            break; }
        case AllowWErrorAndWFatalErrors: {
            serverOpts.options |= Server::AllowWErrorAndWFatalErrors;
            break; }
        case EnableCompilerManager: {
            serverOpts.options |= Server::EnableCompilerManager;
            break; }
        case EnableNDEBUG: {
            serverOpts.options |= Server::EnableNDEBUG;
            break; }
        case Progress: {
            serverOpts.options |= Server::Progress;
            break; }
        case MaxFileMapCacheSize: {
            serverOpts.maxFileMapScopeCacheSize = atoi(value.constData());
            if (serverOpts.maxFileMapScopeCacheSize <= 0) {
                return { String::format<1024>("Invalid argument to -y %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
#ifdef FILEMANAGER_OPT_IN
        case FileManagerWatch: {
            serverOpts.options &= ~Server::NoFileManagerWatch;
            break; }
#else
        case NoFileManagerWatch: {
            serverOpts.options |= Server::NoFileManagerWatch;
            break; }
#endif
        case NoFileManager: {
            serverOpts.options |= Server::NoFileManager;
            break; }
        case NoFileLock: {
            serverOpts.options |= Server::NoFileLock;
            break; }
        case PchEnabled: {
            serverOpts.options |= Server::PCHEnabled;
            break; }
        case NoFilesystemWatcher: {
            serverOpts.options |= Server::NoFileSystemWatch;
            break; }
        case ArgTransform: {
            serverOpts.argTransform = Process::findCommand(value);
            if (!value.isEmpty() && serverOpts.argTransform.isEmpty()) {
                return { String::format<1024>("Invalid argument to -V. Can't resolve %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case NoComments: {
            serverOpts.options |= Server::NoComments;
            break; }
#ifdef RTAGS_HAS_LAUNCHD
        case Launchd: {
            serverOpts.options |= Server::Launchd;
            break; }
#endif
        case InactivityTimeout: {
            inactivityTimeout = atoi(value.constData()); // seconds.
            if (inactivityTimeout <= 0) {
                return { String::format<1024>("Invalid argument to --inactivity-timeout %s", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case Daemon: {
            daemon = true;
            logLevel = LogLevel::None;
            break; }
        case LogFileLogLevel: {
            if (!strcasecmp(value.constData(), "verbose-debug")) {
                logFileLogLevel = LogLevel::VerboseDebug;
            } else if (!strcasecmp(value.constData(), "debug")) {
                logFileLogLevel = LogLevel::Debug;
            } else if (!strcasecmp(value.constData(), "warning")) {
                logFileLogLevel = LogLevel::Warning;
            } else if (!strcasecmp(value.constData(), "error")) {
                logFileLogLevel = LogLevel::Error;
            } else {
                return { String::format<1024>("Unknown log level: %s options are error, warning, debug or verbose-debug", value.constData()),
                         CommandLineParser::Parse_Error };
            }
            break; }
        case WatchSourcesOnly: {
            serverOpts.options |= Server::WatchSourcesOnly;
            break; }
        case DebugLocations: {
            serverOpts.debugLocations << value;
            break; }
        case ValidateFileMaps: {
            serverOpts.options |= Server::ValidateFileMaps;
            break; }
        case TcpPort: {
            serverOpts.tcpPort = atoi(value.constData());
            if (!serverOpts.tcpPort) {
                return { String::format<1024>("Invalid port %s for --tcp-port", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case RPPath: {
            serverOpts.rp = std::move(value);
            if (serverOpts.rp.isFile()) {
                serverOpts.rp.resolve();
            } else {
                return { String::format<1024>("%s is not a file", value.constData()), CommandLineParser::Parse_Error };
            }
            break; }
        case LogTimestamp: {
            logFlags |= LogTimeStamp;
            break; }
        case LogFlushOption: {
            logFlags |= LogFlush;
            break; }
        case SandboxRoot: {
            serverOpts.sandboxRoot = std::move(value);
            if (!serverOpts.sandboxRoot.endsWith('/'))
                serverOpts.sandboxRoot += '/';
            if (!serverOpts.sandboxRoot.resolve() || !serverOpts.sandboxRoot.isDir()) {
                return {
                    String::format<1024>("%s is not a valid directory for sandbox-root",
                                         serverOpts.sandboxRoot.constData()),
                    CommandLineParser::Parse_Error
                };
            }
            break; }
        case NoRealPath: {
            Path::setRealPathEnabled(false);
            serverOpts.options |= Server::NoRealPath;
            break; }
        }

        return { String(), CommandLineParser::Parse_Exec };
    };

    const std::initializer_list<CommandLineParser::Option<CommandLineParser::ConfigOptionType> > configOpts = {
        { CommandLineParser::Config, "config", 'c', CommandLineParser::Required, "Use this file (instead of ~/.rdmrc)." },
        { CommandLineParser::NoRc, "no-rc", 'N', CommandLineParser::NoValue, "Don't load any rc files." }
    };


    const CommandLineParser::ParseStatus status = CommandLineParser::parse<OptionType>(argc, argv, opts, NullFlags, cb, "rdm", configOpts);
    switch (status.status) {
    case CommandLineParser::Parse_Error:
        fprintf(stderr, "%s\n", status.error.constData());
        return 1;
    case CommandLineParser::Parse_Ok:
        return 0;
    case CommandLineParser::Parse_Exec:
        break;
    }

    if (daemon) {
        switch (fork()) {
        case -1:
            fprintf(stderr, "Failed to fork (%d) %s\n", errno, strerror(errno));
            return 1;
        case 0:
            setsid();
            switch (fork()) {
            case -1:
                fprintf(stderr, "Failed to fork (%d) %s\n", errno, strerror(errno));
                return 1;
            case 0:
                break;
            default:
                return 0;
            }
            break;
        default:
            return 0;
        }
    }

    if (serverOpts.excludeFilters.isEmpty())
        serverOpts.excludeFilters = String(DEFAULT_EXCLUDEFILTER).split(';');
    if (serverOpts.compilerWrappers.isEmpty())
        serverOpts.compilerWrappers = String(DEFAULT_COMPILER_WRAPPERS).split(';').toSet();

    serverOpts.tempDir = serverOpts.tempDir.ensureTrailingSlash() + "rdm/";
    Path::rmdir(serverOpts.tempDir);
    serverOpts.tempDir.mkdir(Path::Recursive);
    ::setenv("TMPDIR", serverOpts.tempDir.c_str(), 1);
    if (sigHandler) {
        signal(SIGSEGV, signalHandler);
        signal(SIGBUS, signalHandler);
        signal(SIGILL, signalHandler);
        signal(SIGABRT, signalHandler);
    }

    if (!initLogging(argv[0], logFlags, logLevel, logFile, logFileLogLevel)) {
        fprintf(stderr, "Can't initialize logging with %d %s %s\n",
                logLevel.toInt(), logFile.constData(), logFlags.toString().constData());
        return 1;
    }

#ifdef RTAGS_HAS_LAUNCHD
    if (serverOpts.options & Server::Launchd) {
        // Clamp inactivity timeout. launchd starts to worry if the
        // process runs for less than 10 seconds.

        static const int MIN_INACTIVITY_TIMEOUT = 15; // includes
        // fudge factor.

        if (inactivityTimeout < MIN_INACTIVITY_TIMEOUT) {
            inactivityTimeout = MIN_INACTIVITY_TIMEOUT;
            fprintf(stderr, "launchd mode - clamped inactivity timeout to %d to avoid launchd warnings.\n", inactivityTimeout);
        }
    }
#endif

    std::shared_ptr<EventLoop> loop(new EventLoop);
    loop->init(EventLoop::MainEventLoop|EventLoop::EnableSigIntHandler|EventLoop::EnableSigTermHandler);

    auto server = std::make_shared<Server>();
    if (!serverOpts.tests.isEmpty()) {
        char buf[1024];
        Path path;
        while (true) {
            strcpy(buf, "/tmp/rtags-test-XXXXXX");
            if (!mkdtemp(buf)) {
                fprintf(stderr, "Failed to mkdtemp (%d)\n", errno);
                return 1;
            }
            path = buf;
            path.resolve();
            break;
        }
        serverOpts.dataDir = path;
        strcpy(buf, "/tmp/rtags-sock-XXXXXX");
        const int fd = mkstemp(buf);
        if (fd == -1) {
            fprintf(stderr, "Failed to mkstemp (%d)\n", errno);
            return 1;
        }
        close(fd);
        serverOpts.socketFile = buf;
    }
    serverOpts.dataDir = serverOpts.dataDir.ensureTrailingSlash();

#ifdef HAVE_BACKTRACE
    if (strlen(crashDumpFilePath)) {
        if (crashDumpFilePath[0] != '/') {
            const String f = crashDumpFilePath;
            snprintf(crashDumpFilePath, sizeof(crashDumpFilePath), "%s%s", serverOpts.dataDir.constData(), f.constData());
        }
        snprintf(crashDumpTempFilePath, sizeof(crashDumpTempFilePath), "%s.tmp", crashDumpFilePath);
        Path::mkdir(serverOpts.dataDir);
        crashDumpFile = fopen(crashDumpTempFilePath, "w");
        if (!crashDumpFile) {
            fprintf(stderr, "Couldn't open temp file %s for write (%d)\n", crashDumpTempFilePath, errno);
        }
    }
#endif

    if (!server->init(serverOpts)) {
        cleanupLogging();
        return 1;
    }

    if (!serverOpts.tests.isEmpty()) {
        return server->runTests() ? 0 : 1;
    }

    loop->setInactivityTimeout(inactivityTimeout * 1000);

    loop->exec();
    const int ret = server->exitCode();
    server.reset();
    cleanupLogging();
    return ret;
}
