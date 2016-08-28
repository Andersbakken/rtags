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

#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef OS_Darwin
#include <sys/resource.h>
#endif

#include "rct/EventLoop.h"
#include "rct/FileSystemWatcher.h"
#include "rct/Log.h"
#include "rct/Process.h"
#include "rct/rct-config.h"
#include "rct/Rct.h"
#include "rct/StackBuffer.h"
#include "rct/Thread.h"
#include "rct/ThreadPool.h"
#include "RTags.h"
#include "Server.h"
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#if !defined(HAVE_FSEVENTS) && defined(HAVE_KQUEUE)
#define FILEMANAGER_OPT_IN
#endif

char crashDumpTempFilePath[PATH_MAX];
char crashDumpFilePath[PATH_MAX];
FILE *crashDumpFile = 0;
static void signalHandler(int signal)
{
    enum { SIZE = 1024 };
    void *stack[SIZE];

    fprintf(stderr, "Caught signal %d\n", signal);
#ifdef HAVE_BACKTRACE
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
    }
    if (Server *server = Server::instance())
        server->stopServers();
    _exit(1);
}

#define DEFAULT_EXCLUDEFILTER "*/CMakeFiles/*;*/cmake*/Modules/*;*/conftest.c*;/tmp/*;/private/tmp/*;/private/var/*"
#define DEFAULT_COMPILER_WRAPPERS "ccache"
#define DEFAULT_RP_VISITFILE_TIMEOUT 60000
#define DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE 500
#define DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT 60000
#define DEFAULT_RP_CONNECT_TIMEOUT 0 // won't time out
#define DEFAULT_RP_CONNECT_ATTEMPTS 3
#define DEFAULT_COMPLETION_CACHE_SIZE 10
#define DEFAULT_ERROR_LIMIT 50
#define DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH 3
#define DEFAULT_MAX_CRASH_COUNT 5
#define XSTR(s) #s
#define STR(s) XSTR(s)
#ifdef NDEBUG
#define DEFAULT_SUSPEND_RP "off"
#else
#define DEFAULT_SUSPEND_RP "on"
#endif

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
            unlink(crashDumpTempFilePath);
        }
    }
};

enum ConfigOptionType {
    ConfigNone = 0,
    Config,
    NoRc
};

enum OptionType {
    None = 0,
    Help,
    Version,
    IncludePath,
    Isystem,
    Define,
    LogFile,
    CrashDumpFile,
    SetEnv,
    NoWall,
    Weverything,
    Verbose,
    JobCount,
    HeaderErrorJobCount,
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
    DisallowMultipleSources,
    NoStartupProject,
    NoNoUnknownWarningsOption,
    IgnoreCompiler,
    CompilerWrappers,
    WatchSystemPaths,
    RpVisitFileTimeout,
    RpIndexerMessageTimeout,
    RpConnectTimeout,
    RpConnectAttempts,
    RpNiceValue,
    SuspendRpOnCrash,
    RpLogToSyslog,
    StartSuspended,
    SeparateDebugAndRelease,
    MaxCrashCount,
    CompletionCacheSize,
    CompletionNoFilter,
    CompletionLogs,
    MaxIncludeCompletionDepth,
    AllowWpedantic,
    AllowWErrorAndWFatalErrors,
    EnableCompilerManager,
    EnableNDEBUG,
    Progress,
    MaxFileMapCacheSize,
#ifdef OS_FreeBSD
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
    RpPath,
    LogTimestamp,
    SandboxRoot,
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
    List<String> argCopy;
    List<char*> argList;
    {
        bool norc = false;
        Path rcfile = Path::home() + ".rdmrc";
        opterr = 0;

        StackBuffer<128, char*> originalArgv(argc);
        memcpy(originalArgv, argv, sizeof(char*) * argc);
        /* getopt will molest argv by moving pointers around when it sees
         * fit. Their idea of an optional argument is different from ours so we
         * have to take a copy of argv before they get their sticky fingers all
         * over it.
         *
         * We think this should be okay for an optional argument:
         * -s something
         *
         * They only populate optarg if you do:
         * -ssomething.
         *
         * We don't want to copy argv into argList before processing rc files
         * since command line args should take precedence over things in rc
         * files.
         *
         */

        const struct CommandLineParser::Option<ConfigOptionType> configOpts[] = {
            { Config, "config", 'c', required_argument, "Use this file (instead of ~/.rdmrc)." },
            { NoRc, "no-rc", 'N', no_argument, "Don't load any rc files." }
        };

        CommandLineParser::parse<ConfigOptionType>(argc, argv, configOpts, sizeof(configOpts) / sizeof(configOpts[0]),
                                                   CommandLineParser::IgnoreUnknown, [&norc, &rcfile](ConfigOptionType type) {
                                                       switch (type) {
                                                       case ConfigNone:
                                                           assert(0);
                                                           break;
                                                       case Config:
                                                           rcfile = optarg;
                                                           break;
                                                       case NoRc:
                                                           norc = true;
                                                           break;
                                                       }

                                                       return CommandLineParser::Parse_Exec;
                                                   });

        argList.append(argv[0]);
        if (!norc) {
            String rc = Path("/etc/rdmrc").readAll();
            if (!rc.isEmpty()) {
                for (const String &s : rc.split('\n')) {
                    if (!s.isEmpty() && !s.startsWith('#'))
                        argCopy += s.split(' ');
                }
            }
            if (!rcfile.isEmpty()) {
                rc = rcfile.readAll();
                if (!rc.isEmpty()) {
                    for (const String& s : rc.split('\n')) {
                        if (!s.isEmpty() && !s.startsWith('#'))
                            argCopy += s.split(' ');
                    }
                }
            }
            const int s = argCopy.size();
            for (int i=0; i<s; ++i) {
                String &arg = argCopy.at(i);
                if (!arg.isEmpty())
                    argList.append(arg.data());
            }
        }

        for (int i=1; i<argc; ++i)
            argList.append(originalArgv[i]);
    }

    Server::Options serverOpts;
    serverOpts.socketFile = String::format<128>("%s.rdm", Path::home().constData());
    serverOpts.jobCount = std::max(2, ThreadPool::idealThreadCount());
    serverOpts.headerErrorJobCount = -1;
    serverOpts.rpVisitFileTimeout = DEFAULT_RP_VISITFILE_TIMEOUT;
    serverOpts.rpIndexDataMessageTimeout = DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT;
    serverOpts.rpConnectTimeout = DEFAULT_RP_CONNECT_TIMEOUT;
    serverOpts.rpConnectAttempts = DEFAULT_RP_CONNECT_ATTEMPTS;
    serverOpts.maxFileMapScopeCacheSize = DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE;
    serverOpts.errorLimit = DEFAULT_ERROR_LIMIT;
    serverOpts.rpNiceValue = INT_MIN;
    serverOpts.options = Server::Wall|Server::SpellChecking;
    serverOpts.maxCrashCount = DEFAULT_MAX_CRASH_COUNT;
    serverOpts.completionCacheSize = DEFAULT_COMPLETION_CACHE_SIZE;
    serverOpts.maxIncludeCompletionDepth = DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH;
    serverOpts.rp = defaultRP();
    strcpy(crashDumpFilePath, "crash.dump");
#ifdef OS_FreeBSD
    serverOpts.options |= Server::NoFileManagerWatch;
#endif
    // #ifndef NDEBUG
    //     serverOpts.options |= Server::SuspendRPOnCrash;
    // #endif
    serverOpts.dataDir = String::format<128>("%s.rtags", Path::home().constData());

    const char *logFile = 0;
    Flags<LogFlag> logFlags = DontRotate|LogStderr;
    LogLevel logLevel(LogLevel::Error);
    LogLevel logFileLogLevel(LogLevel::Error);
    bool sigHandler = true;
    assert(Path::home().endsWith('/'));
    int argCount = argList.size();
    char **args = argList.data();
    int inactivityTimeout = 0;

    const struct CommandLineParser::Option<OptionType> opts[] = {
        { None, 0, 0, 0, "Options:" },
        { Help, "help", 'h', no_argument, "Display this page." },
        { Version, "version", 0, no_argument, "Display version." },
        { IncludePath, "include-path", 'I', required_argument, "Add additional include path to clang." },
        { Isystem, "isystem", 's', required_argument, "Add additional system include path to clang." },
        { Define, "define", 'D', required_argument, "Add additional define directive to clang" },
        { LogFile, "log-file", 'L', required_argument, "Log to this file." },
        { CrashDumpFile, "crash-dump-file", 0, required_argument, "File to dump crash log to (default is <datadir>/crash.dump)." },
        { SetEnv, "setenv", 'e', required_argument, "Set this environment variable (--setenv \"foobar=1\")." },
        { NoWall, "no-Wall", 'W', no_argument, "Don't use -Wall." },
        { Weverything, "Weverything", 'u', no_argument, "Use -Weverything." },
        { Verbose, "verbose", 'v', no_argument, "Change verbosity, multiple -v's are allowed." },
        { JobCount, "job-count", 'j', required_argument, String::format("Spawn this many concurrent processes for indexing (default %d).",
                                                                        std::max(2, ThreadPool::idealThreadCount())) },
        { HeaderErrorJobCount, "header-error-job-count", 'H', required_argument, "Allow this many concurrent header error jobs (default std::max(1, --job-count / 2))." },
        { Test, "test", 't', required_argument, "Run this test." },
        { TestTimeout, "test-timeout", 'z', required_argument, "Timeout for test to complete." },
        { CleanSlate, "clean-slate", 'C', no_argument, "Clear out all data." },
        { DisableSigHandler, "disable-sighandler", 'x', no_argument, "Disable signal handler to dump stack for crashes." },
        { Silent, "silent", 'S', no_argument, "No logging to stdout/stderr." },
        { ExcludeFilter, "exclude-filter", 'X', required_argument, "Files to exclude from rdm, default \"" DEFAULT_EXCLUDEFILTER "\"." },
        { SocketFile, "socket-file", 'n', required_argument, "Use this file for the server socket (default ~/.rdm)." },
        { DataDir, "data-dir", 'd', required_argument, "Use this directory to store persistent data (default ~/.rtags)." },
        { IgnorePrintfFixits, "ignore-printf-fixits", 'F', no_argument, "Disregard any clang fixit that looks like it's trying to fix format for printf and friends." },
        { ErrorLimit, "error-limit", 'f', required_argument, "Set error limit to argument (-ferror-limit={arg} (default " STR(DEFAULT_ERROR_LIMIT) ")." },
        { BlockArgument, "block-argument", 'G', required_argument, "Block this argument from being passed to clang. E.g. rdm --block-argument -fno-inline" },
        { NoSpellChecking, "no-spell-checking", 'l', no_argument, "Don't pass -fspell-checking." },
        { LargeByValueCopy, "large-by-value-copy", 'r', required_argument, "Use -Wlarge-by-value-copy=[arg] when invoking clang." },
        { DisallowMultipleSources, "disallow-multiple-sources", 'm', no_argument, "With this setting different sources will be merged for each source file." },
        { NoStartupProject, "no-startup-project", 'o', no_argument, "Don't restore the last current project on startup." },
        { NoNoUnknownWarningsOption, "no-no-unknown-warnings-option", 'Y', no_argument, "Don't pass -Wno-unknown-warning-option." },
        { IgnoreCompiler, "ignore-compiler", 'b', required_argument, "Ignore this compiler." },
        { CompilerWrappers, "compiler-wrappers", 0, required_argument, "Consider these filenames compiler wrappers (split on ;), default " DEFAULT_COMPILER_WRAPPERS "\"." },
        { WatchSystemPaths, "watch-system-paths", 'w', no_argument, "Watch system paths for changes." },
        { RpVisitFileTimeout, "rp-visit-file-timeout", 'Z', required_argument, "Timeout for rp visitfile commands in ms (0 means no timeout) (default " STR(DEFAULT_RP_VISITFILE_TIMEOUT) ")." },
        { RpIndexerMessageTimeout, "rp-indexer-message-timeout", 'T', required_argument, "Timeout for rp indexer-message in ms (0 means no timeout) (default " STR(DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT) ")." },
        { RpConnectTimeout, "rp-connect-timeout", 'O', required_argument, "Timeout for connection from rp to rdm in ms (0 means no timeout) (default " STR(DEFAULT_RP_CONNECT_TIMEOUT) ")." },
        { RpConnectAttempts, "rp-connect-attempts", 0, required_argument, "Number of times rp attempts to connect to rdm before giving up. (default " STR(DEFAULT_RP_CONNECT_ATTEMPTS) ")." },
        { RpNiceValue, "rp-nice-value", 'a', required_argument, "Nice value to use for rp (nice(2)) (default is no nicing)." },
        { SuspendRpOnCrash, "suspend-rp-on-crash", 'q', no_argument, "Suspend rp in SIGSEGV handler (default " DEFAULT_SUSPEND_RP ")." },
        { RpLogToSyslog, "rp-log-to-syslog", 0, no_argument, "Make rp log to syslog." },
        { StartSuspended, "start-suspended", 'Q', no_argument, "Start out suspended (no reindexing enabled)." },
        { SeparateDebugAndRelease, "separate-debug-and-release", 'E', no_argument, "Normally rdm doesn't consider release and debug as different builds. Pass this if you want it to." },
        { MaxCrashCount, "max-crash-count", 'K', required_argument, "Max number of crashes before giving up a sourcefile (default " STR(DEFAULT_MAX_CRASH_COUNT) ")." },
        { CompletionCacheSize, "completion-cache-size", 'i', required_argument, "Number of translation units to cache (default " STR(DEFAULT_COMPLETION_CACHE_SIZE) ")." },
        { CompletionNoFilter, "completion-no-filter", 0, no_argument, "Don't filter private members and destructors from completions." },
        { CompletionLogs, "completion-logs", 0, no_argument, "Log more info about completions." },
        { MaxIncludeCompletionDepth, "max-include-completion-depth", 0, required_argument, "Max recursion depth for header completion (default " STR(DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH) ")." },
        { AllowWpedantic, "allow-Wpedantic", 'P', no_argument, "Don't strip out -Wpedantic. This can cause problems in certain projects." },
        { AllowWErrorAndWFatalErrors, "allow-Werror", 0, no_argument, "Don't strip out -Werror and -Wfatal-error. By default these are stripped out. " },
        { EnableCompilerManager, "enable-compiler-manager", 'R', no_argument, "Query compilers for their actual include paths instead of letting clang use its own." },
        { EnableNDEBUG, "enable-NDEBUG", 'g', no_argument, "Don't remove -DNDEBUG from compile lines." },
        { Progress, "progress", 'p', no_argument, "Report compilation progress in diagnostics output." },
        { MaxFileMapCacheSize, "max-file-map-cache-size", 'y', required_argument, "Max files to cache per query (Should not exceed maximum number of open file descriptors allowed per process) (default " STR(DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE) ")." },
#ifdef FILEMANAGER_OPT_IN
        { FileManagerWatch, "filemanager-watch", 'M', no_argument, "Use a file system watcher for filemanager." },
#else
        { NoFileManagerWatch, "no-filemanager-watch", 'M', no_argument, "Don't use a file system watcher for filemanager." },
#endif
        { NoFileManager, "no-filemanager", 0, no_argument, "Don't scan project directory for files. (rc -P won't work)." },
        { NoFileLock, "no-file-lock", 0, no_argument, "Disable file locking. Not entirely safe but might improve performance on certain systems." },
        { PchEnabled, "pch-enabled", 0, no_argument, "Enable PCH (experimental)." },
        { NoFilesystemWatcher, "no-filesystem-watcher", 'B', no_argument, "Disable file system watching altogether. Reindexing has to be triggered manually." },
        { ArgTransform, "arg-transform", 'V', required_argument, "Use arg to transform arguments. [arg] should be executable with (execv(3))." },
        { NoComments, "no-comments", 0, no_argument, "Don't parse/store doxygen comments." },
#ifdef RTAGS_HAS_LAUNCHD
        { Launchd, "launchd", 0, no_argument, "Run as a launchd job (use launchd API to retrieve socket opened by launchd on rdm's behalf)." },
#endif
        { InactivityTimeout, "inactivity-timeout", 0, required_argument, "Time in seconds after which rdm will quit if there's been no activity (N.B., once rdm has quit, something will need to re-run it!)." },
        { Daemon, "daemon", 0, no_argument, "Run as daemon (detach from terminal)." },
        { LogFileLogLevel, "log-file-log-level", 0, required_argument, "Log level for log file (default is error), options are: error, warning, debug or verbose-debug." },
        { WatchSourcesOnly, "watch-sources-only", 0, no_argument, "Only watch source files (not dependencies)." },
        { DebugLocations, "debug-locations", 0, no_argument, "Set debug locations." },
        { ValidateFileMaps, "validate-file-maps", 0, no_argument, "Spend some time validating project data on startup." },
        { TcpPort, "tcp-port", 0, required_argument, "Listen on this tcp socket (default none)." },
        { RpPath, "rp-path", 0, required_argument, String::format<256>("Path to rp (default %s).", defaultRP().constData()) },
        { LogTimestamp, "log-timestamp", 0, no_argument, "Add timestamp to logs." },
        { SandboxRoot, "sandbox-root",  0, required_argument, "Create index using relative paths by stripping dir (enables copying of tag index db files without need to reindex)." },
        { Noop, "config", 'c', required_argument, "Use this file (instead of ~/.rdmrc)." },
        { Noop, "no-rc", 'N', no_argument, "Don't load any rc files." }
    };

    std::function<CommandLineParser::ParseStatus(OptionType type)> cb;
    cb = [&](OptionType type) {
        switch (type) {
        case None:
        case Noop:
            break;
        case Help:
            CommandLineParser::help(stdout, Rct::executablePath().fileName(), opts, sizeof(opts) / sizeof(opts[0]));
            return CommandLineParser::Parse_Ok;
        case Version:
            fprintf(stdout, "%s\n", RTags::versionString().constData());
            return CommandLineParser::Parse_Ok;
        case IncludePath:
            serverOpts.includePaths.append(Source::Include(Source::Include::Type_Include, Path::resolved(optarg)));
            break;
        case Isystem:
            serverOpts.includePaths.append(Source::Include(Source::Include::Type_System, Path::resolved(optarg)));
            break;

        case Define: {
            const char *eq = strchr(optarg, '=');
            Source::Define def;
            if (!eq) {
                def.define = optarg;
            } else {
                def.define = String(optarg, eq - optarg);
                def.value = eq + 1;
            }
            serverOpts.defines.append(def);
            break; }
        case LogFile:
            logFile = optarg;
            logLevel = LogLevel::None;
            break;
        case CrashDumpFile:
            strcpy(crashDumpFilePath, optarg);
            break;
        case SetEnv:
            putenv(optarg);
            break;
        case NoWall:
            serverOpts.options &= ~Server::Wall;
            break;
        case Weverything:
            serverOpts.options |= Server::Weverything;
            break;
        case Verbose:
            if (logLevel != LogLevel::None)
                ++logLevel;
            break;
        case JobCount: {
            bool ok;
            serverOpts.jobCount = String(optarg).toULong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -j %s. -j must be a positive integer.\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break; }
        case HeaderErrorJobCount: {
            bool ok;
            serverOpts.headerErrorJobCount = String(optarg).toULong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -H %s. -H must be a positive integer.\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break; }
        case Test: {
            Path test(optarg);
            if (!test.resolve() || !test.isFile()) {
                fprintf(stderr, "%s doesn't seem to be a file\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            serverOpts.tests += test;
            break; }
        case TestTimeout:
            serverOpts.testTimeout = atoi(optarg);
            if (serverOpts.testTimeout <= 0) {
                fprintf(stderr, "Invalid argument to -z %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case CleanSlate:
            serverOpts.options |= Server::ClearProjects;
            break;
        case DisableSigHandler:
            sigHandler = false;
            break;
        case Silent:
            logLevel = LogLevel::None;
            break;
        case ExcludeFilter:
            serverOpts.excludeFilters += String(optarg).split(';');
            break;
        case SocketFile:
            serverOpts.socketFile = optarg;
            serverOpts.socketFile.resolve();
            break;
        case DataDir:
            serverOpts.dataDir = String::format<128>("%s", Path::resolved(optarg).constData());
            break;
        case IgnorePrintfFixits:
            serverOpts.options |= Server::IgnorePrintfFixits;
            break;
        case ErrorLimit: {
            bool ok;
            serverOpts.errorLimit = String(optarg).toULong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to --error-limit %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break; }
        case BlockArgument:
            serverOpts.blockedArguments << optarg;
            break;
        case NoSpellChecking:
            serverOpts.options &= ~Server::SpellChecking;
            break;
        case LargeByValueCopy: {
            int large = atoi(optarg);
            if (large <= 0) {
                fprintf(stderr, "Can't parse argument to -r %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            serverOpts.defaultArguments.append("-Wlarge-by-value-copy=" + String(optarg)); // ### not quite working
            break; }
        case DisallowMultipleSources:
            serverOpts.options |= Server::DisallowMultipleSources;
            break;
        case NoStartupProject:
            serverOpts.options |= Server::NoStartupCurrentProject;
            break;
        case NoNoUnknownWarningsOption:
            serverOpts.options |= Server::NoNoUnknownWarningsOption;
            break;
        case IgnoreCompiler:
            serverOpts.ignoredCompilers.insert(Path::resolved(optarg));
            break;
        case CompilerWrappers:
            serverOpts.compilerWrappers = String(optarg).split(";", String::SkipEmpty).toSet();
            break;
        case WatchSystemPaths:
            serverOpts.options |= Server::WatchSystemPaths;
            break;
        case RpVisitFileTimeout:
            serverOpts.rpVisitFileTimeout = atoi(optarg);
            if (serverOpts.rpVisitFileTimeout < 0) {
                fprintf(stderr, "Invalid argument to -Z %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            if (!serverOpts.rpVisitFileTimeout)
                serverOpts.rpVisitFileTimeout = -1;
            break;
        case RpIndexerMessageTimeout:
            serverOpts.rpIndexDataMessageTimeout = atoi(optarg);
            if (serverOpts.rpIndexDataMessageTimeout <= 0) {
                fprintf(stderr, "Can't parse argument to -T %s.\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case RpConnectTimeout:
            serverOpts.rpConnectTimeout = atoi(optarg);
            if (serverOpts.rpConnectTimeout < 0) {
                fprintf(stderr, "Invalid argument to -O %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case RpConnectAttempts:
            serverOpts.rpConnectAttempts = atoi(optarg);
            if (serverOpts.rpConnectAttempts <= 0) {
                fprintf(stderr, "Invalid argument to --rp-connect-attempts %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case RpNiceValue: {
            bool ok;
            serverOpts.rpNiceValue = String(optarg).toLong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -a %s.\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break; }
        case SuspendRpOnCrash:
            serverOpts.options |= Server::SuspendRPOnCrash;
            break;
        case RpLogToSyslog:
            serverOpts.options |= Server::RPLogToSyslog;
            break;
        case StartSuspended:
            serverOpts.options |= Server::StartSuspended;
            break;
        case SeparateDebugAndRelease:
            serverOpts.options |= Server::SeparateDebugAndRelease;
            break;
        case MaxCrashCount:
            serverOpts.maxCrashCount = atoi(optarg);
            if (serverOpts.maxCrashCount <= 0) {
                fprintf(stderr, "Invalid argument to -K %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case CompletionCacheSize:
            serverOpts.completionCacheSize = atoi(optarg);
            if (serverOpts.completionCacheSize <= 0) {
                fprintf(stderr, "Invalid argument to -i %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case CompletionNoFilter:
            serverOpts.options |= Server::CompletionsNoFilter;
            break;
        case CompletionLogs:
            serverOpts.options |= Server::CompletionLogs;
            break;
        case MaxIncludeCompletionDepth:
            serverOpts.maxIncludeCompletionDepth = strtoul(optarg, 0, 10);
            break;
        case AllowWpedantic:
            serverOpts.options |= Server::AllowPedantic;
            break;
        case AllowWErrorAndWFatalErrors:
            serverOpts.options |= Server::AllowWErrorAndWFatalErrors;
            break;
        case EnableCompilerManager:
            serverOpts.options |= Server::EnableCompilerManager;
            break;
        case EnableNDEBUG:
            serverOpts.options |= Server::EnableNDEBUG;
            break;
        case Progress:
            serverOpts.options |= Server::Progress;
            break;
        case MaxFileMapCacheSize:
            serverOpts.maxFileMapScopeCacheSize = atoi(optarg);
            if (serverOpts.maxFileMapScopeCacheSize <= 0) {
                fprintf(stderr, "Invalid argument to -y %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
#ifdef FILEMANAGER_OPT_IN
        case FileManagerWatch:
            serverOpts.options &= ~Server::NoFileManagerWatch;
            break;
#else
        case NoFileManagerWatch:
            serverOpts.options |= Server::NoFileManagerWatch;
            break;
#endif
        case NoFileManager:
            serverOpts.options |= Server::NoFileManager;
            break;
        case NoFileLock:
            serverOpts.options |= Server::NoFileLock;
            break;
        case PchEnabled:
            serverOpts.options |= Server::PCHEnabled;
            break;
        case NoFilesystemWatcher:
            serverOpts.options |= Server::NoFileSystemWatch;
            break;
        case ArgTransform:
            serverOpts.argTransform = Process::findCommand(optarg);
            if (strlen(optarg) && serverOpts.argTransform.isEmpty()) {
                fprintf(stderr, "Invalid argument to -V. Can't resolve %s", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case NoComments:
            serverOpts.options |= Server::NoComments;
            break;
#ifdef RTAGS_HAS_LAUNCHD
        case Launchd:
            serverOpts.options |= Server::Launchd;
            break;
#endif
        case InactivityTimeout:
            inactivityTimeout = atoi(optarg); // seconds.
            if (inactivityTimeout <= 0) {
                fprintf(stderr, "Invalid argument to --inactivity-timeout %s\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case Daemon:
            daemon = true;
            logLevel = LogLevel::None;
            break;
        case LogFileLogLevel:
            if (!strcasecmp(optarg, "verbose-debug")) {
                logFileLogLevel = LogLevel::VerboseDebug;
            } else if (!strcasecmp(optarg, "debug")) {
                logFileLogLevel = LogLevel::Debug;
            } else if (!strcasecmp(optarg, "warning")) {
                logFileLogLevel = LogLevel::Warning;
            } else if (!strcasecmp(optarg, "error")) {
                logFileLogLevel = LogLevel::Error;
            } else {
                fprintf(stderr, "Unknown log level: %s options are error, warning, debug or verbose-debug\n",
                        optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case WatchSourcesOnly:
            serverOpts.options |= Server::WatchSourcesOnly;
            break;
        case DebugLocations:
            if (!strcmp(optarg, "clear") || !strcmp(optarg, "none")) {
                serverOpts.debugLocations.clear();
            } else {
                serverOpts.debugLocations << optarg;
            }
            break;
        case ValidateFileMaps:
            serverOpts.options |= Server::ValidateFileMaps;
            break;
        case TcpPort:
            serverOpts.tcpPort = atoi(optarg);
            if (!serverOpts.tcpPort) {
                fprintf(stderr, "Invalid port %s for --tcp-port\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case RpPath:
            serverOpts.rp = optarg;
            if (serverOpts.rp.isFile()) {
                serverOpts.rp.resolve();
            } else {
                fprintf(stderr, "%s is not a file\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break;
        case LogTimestamp:
            logFlags |= LogTimeStamp;
            break;
        case SandboxRoot: {
            const int len = strlen(optarg);
            if (optarg[len-1] != '/') {
                std::string tmparg = optarg;
                tmparg += '/';
                serverOpts.sandboxRoot = tmparg.c_str();
            } else {
                serverOpts.sandboxRoot = optarg;
            }
            if (!serverOpts.sandboxRoot.resolve() || !serverOpts.sandboxRoot.isDir()) {
                fprintf(stderr, "%s is not a valid directory for sandbox-root\n", optarg);
                return CommandLineParser::Parse_Error;
            }
            break; }
        }

        return CommandLineParser::Parse_Exec;
    };

    switch (CommandLineParser::parse<OptionType>(argCount, args, opts, sizeof(opts) / sizeof(opts[0]), NullFlags, cb)) {
    case CommandLineParser::Parse_Error:
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

    if (!serverOpts.headerErrorJobCount) {
        serverOpts.headerErrorJobCount = std::max<size_t>(1, serverOpts.jobCount / 2);
    } else {
        serverOpts.headerErrorJobCount = std::min(serverOpts.headerErrorJobCount, serverOpts.jobCount);
    }

    if (sigHandler) {
        signal(SIGSEGV, signalHandler);
        signal(SIGBUS, signalHandler);
        signal(SIGILL, signalHandler);
        signal(SIGABRT, signalHandler);
    }

    // Shell-expand logFile
    Path logPath(logFile); logPath.resolve();

    if (!initLogging(argv[0], logFlags, logLevel, logPath.constData(), logFileLogLevel)) {
        fprintf(stderr, "Can't initialize logging with %d %s %s\n",
                logLevel.toInt(), logFile ? logFile : "", logFlags.toString().constData());
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

    EventLoop::SharedPtr loop(new EventLoop);
    loop->init(EventLoop::MainEventLoop|EventLoop::EnableSigIntHandler|EventLoop::EnableSigTermHandler);

    std::shared_ptr<Server> server(new Server);
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
        serverOpts.socketFile.resolve();
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
            return 1;
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


