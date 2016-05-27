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

#define EXCLUDEFILTER_DEFAULT "*/CMakeFiles/*;*/cmake*/Modules/*;*/conftest.c*;/tmp/*"
#define DEFAULT_RP_VISITFILE_TIMEOUT 60000
#define DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE 500
#define DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT 60000
#define DEFAULT_RP_CONNECT_TIMEOUT 0 // won't time out
#define DEFAULT_RP_CONNECT_ATTEMPTS 3
#define DEFAULT_COMPLETION_CACHE_SIZE 10
#define DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH 3
#define DEFAULT_MAX_CRASH_COUNT 5
#define XSTR(s) #s
#define STR(s) XSTR(s)
static size_t defaultStackSize = 0;
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

static void usage(FILE *f)
{
    fprintf(f,
            "\nUsage: rdm [...options...]\n\n"
            "  --help|-h                                  Display this page.\n"
            "  --version                                  Display version.\n"

            "\nServer options:\n"
            "  --clear-project-caches|-C                  Clear out project caches.\n"
            "  --test|-t [arg]                            Run this test.\n"
            "  --test-timeout|-z [arg]                    Timeout for test to complete.\n"
            "  --completion-cache-size|-i [arg]           Number of translation units to cache (default " STR(DEFAULT_COMPLETION_CACHE_SIZE) ").\n"
            "  --completion-no-filter                     Don't filter private members and destructors from completions.\n"
            "  --max-include-completion-depth [arg]       Max recursion depth for header completion (default " STR(DEFAULT_MAX_INCLUDE_COMPLETION_DEPTH) ").\n"
            "  --config|-c [arg]                          Use this file instead of ~/.rdmrc.\n"
            "  --data-dir|-d [arg]                        Use this directory to store persistent data (default ~/.rtags).\n"
            "  --daemon                                   Run as daemon (detach from terminal).\n"
            "  --disable-sighandler|-x                    Disable signal handler to dump stack for crashes.\n"
            "  --disallow-multiple-sources|-m             With this setting different sources will be merged for each source file.\n"
            "  --enable-NDEBUG|-g                         Don't remove -DNDEBUG from compile lines.\n"
            "  --enable-compiler-manager|-R               Query compilers for their actual include paths instead of letting clang use its own.\n"
            "  --exclude-filter|-X [arg]                  Files to exclude from rdm, default \"" EXCLUDEFILTER_DEFAULT "\".\n"
            "  --extra-compilers|-U [arg]                 Override additional known compilers.\n"

#ifdef FILEMANAGER_OPT_IN
            "  --filemanager-watch|-M                     Use a file system watcher for filemanager.\n"
#else
            "  --no-filemanager-watch|-M                  Don't use a file system watcher for filemanager.\n"
#endif
            "  --no-filemanager                           Don't scan project directory for files. (rc -P won't work)\n"
            "  --watch-sources-only                       Only watch source files (not dependencies).\n"
            "  --job-count|-j [arg]                       Spawn this many concurrent processes for indexing (default %d).\n"
            "  --header-error-job-count|-H [arg]          Allow this many concurrent header error jobs (default std::max(1, --job-count / 2)).\n"
            "  --log-file|-L [arg]                        Log to this file.\n"
            "  --log-file-log-level [arg]                 Log level for log file (default is error).\n"
            "  --crash-dump-file [arg]                    File to dump crash log to (default is <datadir>/crash.dump).\n"
            "                                             options are: error, warning, debug or verbose-debug.\n"
            "  --sandbox-root [dir]                       Create index using relative paths by stripping dir (enables copying of tag index db files without need to reindex)\n"
#ifndef OS_FreeBSD
#endif
            "  --no-filesystem-watcher|-B                 Disable file system watching altogether. Reindexing has to happen manually.\n"
            "  --no-file-lock                             Disable file locking. Not entirely safe but might improve performance on certain systems.\n"
            "  --no-rc|-N                                 Don't load any rc files.\n"
            "  --no-startup-project|-o                    Don't restore the last current project on startup.\n"
            "  --rp-connect-timeout|-O [arg]              Timeout for connection from rp to rdm in ms (0 means no timeout) (default " STR(DEFAULT_RP_CONNECT_TIMEOUT) ").\n"
            "  --rp-connect-attempts [arg]                Number of times rp attempts to connect to rdm before giving up. (default " STR(DEFAULT_RP_CONNECT_ATTEMPTS) ").\n"
            "  --rp-indexer-message-timeout|-T [arg]      Timeout for rp indexer-message in ms (0 means no timeout) (default " STR(DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT) ").\n"
            "  --rp-nice-value|-a [arg]                   Nice value to use for rp (nice(2)) (default is no nicing).\n"
            "  --rp-visit-file-timeout|-Z [arg]           Timeout for rp visitfile commands in ms (0 means no timeout) (default " STR(DEFAULT_RP_VISITFILE_TIMEOUT) ").\n"
            "  --separate-debug-and-release|-E            Normally rdm doesn't consider release and debug as different builds. Pass this if you want it to.\n"
            "  --setenv|-e [arg]                          Set this environment variable (--setenv \"foobar=1\").\n"
            "  --silent|-S                                No logging to stdout.\n"
            "  --socket-file|-n [arg]                     Use this file for the server socket (default ~/.rdm).\n"
            "  --tcp-port [arg]                           Listen on this tcp socket (default none).\n"
            "  --start-suspended|-Q                       Start out suspended (no reindexing enabled).\n"
            "  --suspend-rp-on-crash|-q                   Suspend rp in SIGSEGV handler (default " DEFAULT_SUSPEND_RP ").\n"
            "  --rp-log-to-syslog                         Make rp log to syslog\n"
            "  --log-timestamp                            Add timestamp to logs\n"
            "  --thread-stack-size|-k [arg]               Set stack size for threadpool to this (default %zu).\n"
            "  --verbose|-v                               Change verbosity, multiple -v's are allowed.\n"
            "  --watch-system-paths|-w                    Watch system paths for changes.\n"
            "  --block-argument|-G [arg]                  Block this argument from being passed to clang. E.g. rdm --block-argument -fno-inline\n"
            "  --progress|-p                              Report compilation progress in diagnostics output.\n"
#ifdef RTAGS_HAS_LAUNCHD
            "  --launchd                                  Run as a launchd job (use launchd API to retrieve socket opened by launchd on rdm's behalf).\n"
#endif
            "  --inactivity-timeout [arg]                 Time in seconds after which rdm will quit if there's been no activity (N.B., once rdm has quit, something will need to re-run it!).\n"
            "\nCompiling/Indexing options:\n"
            "  --allow-Wpedantic|-P                       Don't strip out -Wpedantic. This can cause problems in certain projects.\n"
            "  --define|-D [arg]                          Add additional define directive to clang.\n"
            "  --ignore-printf-fixits|-F                  Disregard any clang fixit that looks like it's trying to fix format for printf and friends.\n"
            "  --include-path|-I [arg]                    Add additional include path to clang.\n"
            "  --isystem|-s [arg]                         Add additional system include path to clang.\n"
            "  --Weverything|-u                           Use -Weverything.\n"
            "  --no-Wall|-W                               Don't use -Wall.\n"
            "  --no-no-unknown-warnings-option|-Y         Don't pass -Wno-unknown-warning-option\n"
            "  --no-spell-checking|-l                     Don't pass -fspell-checking.\n"
            "  --no-unlimited-error|-f                    Don't pass -ferror-limit=0 to clang.\n"
            "  --Wlarge-by-value-copy|-r [arg]            Use -Wlarge-by-value-copy=[arg] when invoking clang.\n"
            "  --max-file-map-cache-size|-y [arg]         Max files to cache per query (Should not exceed maximum number of open file descriptors allowed per process) (default " STR(DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE) ").\n"
            "  --no-comments                              Don't parse/store doxygen comments.\n"
            "  --arg-transform|-V [arg]                   Use arg to transform arguments. [arg] should be a executable with (execv(3)).\n"
            "  --debug-locations [arg]                    Set debug locations.\n"
            "  --validate-file-maps                       Spend some time validating project data on startup.\n"
            "  --pch-enabled                              Enable PCH (experimental).\n"
            "  --rp-path [path]                           Path to rp (default %s).\n"
            , std::max(2, ThreadPool::idealThreadCount()), defaultStackSize, defaultRP().constData());
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

    {
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_getstacksize(&attr, &defaultStackSize);
        pthread_attr_destroy(&attr);
        if (defaultStackSize < 1024 * 1024 * 4) { // 4 megs should be enough for everyone right?
            defaultStackSize = 1024 * 1024 * 4;
        }
    }

    Rct::findExecutablePath(*argv);

    struct option opts[] = {
        { "help", no_argument, 0, 'h' },
        { "version", no_argument, 0, 2 },
        { "include-path", required_argument, 0, 'I' },
        { "isystem", required_argument, 0, 's' },
        { "define", required_argument, 0, 'D' },
        { "log-file", required_argument, 0, 'L' },
        { "crash-dump-file", required_argument, 0, 19 },
        { "setenv", required_argument, 0, 'e' },
        { "no-Wall", no_argument, 0, 'W' },
        { "Weverything", no_argument, 0, 'u' },
        { "cache-AST", required_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "job-count", required_argument, 0, 'j' },
        { "header-error-job-count", required_argument, 0, 'H' },
        { "test", required_argument, 0, 't' },
        { "test-timeout", required_argument, 0, 'z' },
        { "clean-slate", no_argument, 0, 'C' },
        { "disable-sighandler", no_argument, 0, 'x' },
        { "silent", no_argument, 0, 'S' },
        { "exclude-filter", required_argument, 0, 'X' },
        { "socket-file", required_argument, 0, 'n' },
        { "config", required_argument, 0, 'c' },
        { "no-rc", no_argument, 0, 'N' },
        { "data-dir", required_argument, 0, 'd' },
        { "ignore-printf-fixits", no_argument, 0, 'F' },
        { "no-unlimited-errors", no_argument, 0, 'f' },
        { "block-argument", required_argument, 0, 'G' },
        { "no-spell-checking", no_argument, 0, 'l' },
        { "large-by-value-copy", required_argument, 0, 'r' },
        { "disallow-multiple-sources", no_argument, 0, 'm' },
        { "no-startup-project", no_argument, 0, 'o' },
        { "no-no-unknown-warnings-option", no_argument, 0, 'Y' },
        { "ignore-compiler", required_argument, 0, 'b' },
        { "watch-system-paths", no_argument, 0, 'w' },
        { "rp-visit-file-timeout", required_argument, 0, 'Z' },
        { "rp-indexer-message-timeout", required_argument, 0, 'T' },
        { "rp-connect-timeout", required_argument, 0, 'O' },
        { "rp-connect-attempts", required_argument, 0, 3 },
        { "rp-nice-value", required_argument, 0, 'a' },
        { "thread-stack-size", required_argument, 0, 'k' },
        { "suspend-rp-on-crash", no_argument, 0, 'q' },
        { "rp-log-to-syslog", no_argument, 0, 7 },
        { "start-suspended", no_argument, 0, 'Q' },
        { "separate-debug-and-release", no_argument, 0, 'E' },
        { "max-crash-count", required_argument, 0, 'K' },
        { "completion-cache-size", required_argument, 0, 'i' },
        { "completion-no-filter", no_argument, 0, 8 },
        { "max-include-completion-depth", required_argument, 0, 21 },
        { "extra-compilers", required_argument, 0, 'U' },
        { "allow-Wpedantic", no_argument, 0, 'P' },
        { "enable-compiler-manager", no_argument, 0, 'R' },
        { "enable-NDEBUG", no_argument, 0, 'g' },
        { "progress", no_argument, 0, 'p' },
        { "max-file-map-cache-size", required_argument, 0, 'y' },
#ifdef OS_FreeBSD
        { "filemanager-watch", no_argument, 0, 'M' },
#else
        { "no-filemanager-watch", no_argument, 0, 'M' },
#endif
        { "no-filemanager", no_argument, 0, 15 },
        { "no-file-lock", no_argument, 0, 13 },
        { "pch-enabled", no_argument, 0, 14 },
        { "no-filesystem-watcher", no_argument, 0, 'B' },
        { "arg-transform", required_argument, 0, 'V' },
        { "no-comments", no_argument, 0, 1 },
#ifdef RTAGS_HAS_LAUNCHD
        { "launchd", no_argument, 0, 4 },
#endif
        { "inactivity-timeout", required_argument, 0, 5 },
        { "daemon", no_argument, 0, 6 },
        { "log-file-log-level", required_argument, 0, 9 },
        { "watch-sources-only", no_argument, 0, 10 },
        { "debug-locations", no_argument, 0, 11 },
        { "validate-file-maps", no_argument, 0, 16 },
        { "tcp-port", required_argument, 0, 12 },
        { "rp-path", required_argument, 0, 17 },
        { "log-timestamp", no_argument, 0, 18 },
        { "sandbox-root", required_argument, 0, 20 },
        { 0, 0, 0, 0 }
    };
    const String shortOptions = Rct::shortOptions(opts);
    if (getenv("RTAGS_DUMP_UNUSED")) {
        String unused;
        for (int i=0; i<26; ++i) {
            if (!shortOptions.contains('a' + i))
                unused.append('a' + i);
            if (!shortOptions.contains('A' + i))
                unused.append('A' + i);
        }
        printf("Unused: %s\n", unused.constData());
        for (int i=0; opts[i].name; ++i) {
            if (opts[i].name) {
                if (!opts[i].val) {
                    printf("No shortoption for %s\n", opts[i].name);
                } else if (opts[i].name[0] != opts[i].val) {
                    printf("Not ideal option for %s|%c\n", opts[i].name, opts[i].val);
                }
            }
        }
        return 0;
    }

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

        while (true) {
            const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
            if (c == -1)
                break;
            switch (c) {
            case 'N':
                norc = true;
                break;
            case 'c':
                rcfile = optarg;
                break;
            default:
                break;
            }
        }
        opterr = 1;
        argList.append(argv[0]);
        if (!norc) {
            String rc = Path("/etc/rdmrc").readAll();
            if (!rc.isEmpty()) {
                for (const String& s : rc.split('\n')) {
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

        optind = 1;
    }

    Server::Options serverOpts;
    serverOpts.threadStackSize = defaultStackSize;
    serverOpts.socketFile = String::format<128>("%s.rdm", Path::home().constData());
    serverOpts.jobCount = std::max(2, ThreadPool::idealThreadCount());
    serverOpts.headerErrorJobCount = -1;
    serverOpts.rpVisitFileTimeout = DEFAULT_RP_VISITFILE_TIMEOUT;
    serverOpts.rpIndexDataMessageTimeout = DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT;
    serverOpts.rpConnectTimeout = DEFAULT_RP_CONNECT_TIMEOUT;
    serverOpts.rpConnectAttempts = DEFAULT_RP_CONNECT_ATTEMPTS;
    serverOpts.maxFileMapScopeCacheSize = DEFAULT_RDM_MAX_FILE_MAP_CACHE_SIZE;
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
    bool defaultDataDir = true;
    int inactivityTimeout = 0;

    while (true) {
        const int c = getopt_long(argCount, args, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 'N':
        case 'c':
            // ignored
            break;
        case 'S':
            logLevel = LogLevel::None;
            break;
        case 'X':
            serverOpts.excludeFilters += String(optarg).split(';');
            break;
        case 'G':
            serverOpts.blockedArguments << optarg;
            break;
        case 1:
            serverOpts.options |= Server::NoComments;
            break;
        case 10:
            serverOpts.options |= Server::WatchSourcesOnly;
            break;
        case 11:
            if (!strcmp(optarg, "clear") || !strcmp(optarg, "none")) {
                serverOpts.debugLocations.clear();
            } else {
                serverOpts.debugLocations << optarg;
            }
            break;
        case 12:
            serverOpts.tcpPort = atoi(optarg);
            if (!serverOpts.tcpPort) {
                fprintf(stderr, "Invalid port %s for --tcp-port\n", optarg);
                return 1;
            }
            break;
        case 13:
            serverOpts.options |= Server::NoFileLock;
            break;
        case 14:
            serverOpts.options |= Server::PCHEnabled;
            break;
        case 15:
            serverOpts.options |= Server::NoFileManager;
            break;
        case 16:
            serverOpts.options |= Server::ValidateFileMaps;
            break;
        case 17:
            serverOpts.rp = optarg;
            if (serverOpts.rp.isFile())
                serverOpts.rp.resolve();
            break;
        case 18:
            logFlags |= LogTimeStamp;
            break;
        case 19:
            strcpy(crashDumpFilePath, optarg);
            break;
        case 20:
            {
                auto len = strlen(optarg);
                if (optarg[len-1] != '/') {
                    std::string tmparg = optarg;
                    tmparg += '/';
                    serverOpts.sandboxRoot = tmparg.c_str();
                } else {
                    serverOpts.sandboxRoot = optarg;
                }
                if (!serverOpts.sandboxRoot.resolve() || !serverOpts.sandboxRoot.isDir()) {
                    fprintf(stderr, "%s is not a valid directory for sandbox-root\n", optarg);
                    return 1;
                }
            }
            break;
        case 2:
            fprintf(stdout, "%s\n", RTags::versionString().constData());
            return 0;
        case 6:
            daemon = true;
            logLevel = LogLevel::None;
            break;
        case 9:
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
                return 1;
            }
            break;
        case 'U':
            serverOpts.extraCompilers.append(std::regex(optarg));
            break;
        case 'E':
            serverOpts.options |= Server::SeparateDebugAndRelease;
            break;
        case 'g':
            serverOpts.options |= Server::EnableNDEBUG;
            break;
        case 'Q':
            serverOpts.options |= Server::StartSuspended;
            break;
        case 'Z':
            serverOpts.rpVisitFileTimeout = atoi(optarg);
            if (serverOpts.rpVisitFileTimeout < 0) {
                fprintf(stderr, "Invalid argument to -Z %s\n", optarg);
                return 1;
            }
            if (!serverOpts.rpVisitFileTimeout)
                serverOpts.rpVisitFileTimeout = -1;
            break;
        case 'y':
            serverOpts.maxFileMapScopeCacheSize = atoi(optarg);
            if (serverOpts.maxFileMapScopeCacheSize <= 0) {
                fprintf(stderr, "Invalid argument to -y %s\n", optarg);
                return 1;
            }
            break;
        case 'O':
            serverOpts.rpConnectTimeout = atoi(optarg);
            if (serverOpts.rpConnectTimeout < 0) {
                fprintf(stderr, "Invalid argument to -O %s\n", optarg);
                return 1;
            }
            break;
        case 3:
            serverOpts.rpConnectAttempts = atoi(optarg);
            if (serverOpts.rpConnectAttempts <= 0) {
                fprintf(stderr, "Invalid argument to --rp-connect-attempts %s\n", optarg);
                return 1;
            }
            break;
        case 'k':
            serverOpts.threadStackSize = atoi(optarg);
            if (serverOpts.threadStackSize < 0) {
                fprintf(stderr, "Invalid argument to -k %s\n", optarg);
                return 1;
            }
            break;
        case 'b':
            serverOpts.ignoredCompilers.insert(Path::resolved(optarg));
            break;
        case 't': {
            Path test(optarg);
            if (!test.resolve() || !test.isFile()) {
                fprintf(stderr, "%s doesn't seem to be a file\n", optarg);
                return 1;
            }
            serverOpts.tests += test;
            break; }
        case 'z':
            serverOpts.testTimeout = atoi(optarg);
            if (serverOpts.testTimeout <= 0) {
                fprintf(stderr, "Invalid argument to -z %s\n", optarg);
                return 1;
            }
            break;
        case 'n':
            serverOpts.socketFile = optarg;
            serverOpts.socketFile.resolve();
            break;
        case 'd':
            defaultDataDir = false;
            serverOpts.dataDir = String::format<128>("%s", Path::resolved(optarg).constData());
            break;
        case 'h':
            usage(stdout);
            return 0;
        case 'Y':
            serverOpts.options |= Server::NoNoUnknownWarningsOption;
            break;
        case 'p':
            serverOpts.options |= Server::Progress;
            break;
        case 'R':
            serverOpts.options |= Server::EnableCompilerManager;
            break;
        case 'm':
            serverOpts.options |= Server::DisallowMultipleSources;
            break;
        case 'o':
            serverOpts.options |= Server::NoStartupCurrentProject;
            break;
        case 'w':
            serverOpts.options |= Server::WatchSystemPaths;
            break;
        case 'q':
            serverOpts.options |= Server::SuspendRPOnCrash;
            break;
        case 'M':
#ifdef OS_FreeBSD
            serverOpts.options &= ~Server::NoFileManagerWatch;
#else
            serverOpts.options |= Server::NoFileManagerWatch;
#endif
            break;
        case 'B':
            serverOpts.options |= Server::NoFileSystemWatch;
            break;
        case 'V':
            serverOpts.argTransform = Process::findCommand(optarg);
            if (strlen(optarg) && serverOpts.argTransform.isEmpty()) {
                fprintf(stderr, "Invalid argument to -V. Can't resolve %s", optarg);
                return 1;
            }

            break;
      case 'F':
            serverOpts.options |= Server::IgnorePrintfFixits;
            break;
        case 'f':
            serverOpts.options |= Server::NoUnlimitedErrors;
            break;
        case 'l':
            serverOpts.options &= ~Server::SpellChecking;
            break;
        case 'W':
            serverOpts.options &= ~Server::Wall;
            break;
        case 'u':
            serverOpts.options |= Server::Weverything;
            break;
        case 'P':
            serverOpts.options |= Server::AllowPedantic;
            break;
        case 'C':
            serverOpts.options |= Server::ClearProjects;
            break;
        case 'e':
            putenv(optarg);
            break;
        case 'x':
            sigHandler = false;
            break;
        case 'K':
            serverOpts.maxCrashCount = atoi(optarg);
            if (serverOpts.maxCrashCount <= 0) {
                fprintf(stderr, "Invalid argument to -K %s\n", optarg);
                return 1;
            }
            break;
        case 'i':
            serverOpts.completionCacheSize = atoi(optarg);
            if (serverOpts.completionCacheSize <= 0) {
                fprintf(stderr, "Invalid argument to -i %s\n", optarg);
                return 1;
            }
            break;
        case 21:
            serverOpts.maxIncludeCompletionDepth = strtoul(optarg, 0, 10);
            break;
        case 'T':
            serverOpts.rpIndexDataMessageTimeout = atoi(optarg);
            if (serverOpts.rpIndexDataMessageTimeout <= 0) {
                fprintf(stderr, "Can't parse argument to -T %s.\n", optarg);
                return 1;
            }
            break;
        case 'a': {
            bool ok;
            serverOpts.rpNiceValue = String(optarg).toLong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -a %s.\n", optarg);
                return 1;
            }
            break; }
        case 'j': {
            bool ok;
            serverOpts.jobCount = String(optarg).toULong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -j %s. -j must be a positive integer.\n", optarg);
                return 1;
            }
            break; }
        case 'H': {
            bool ok;
            serverOpts.headerErrorJobCount = String(optarg).toULong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -H %s. -H must be a positive integer.\n", optarg);
                return 1;
            }
            break; }
        case 'r': {
            int large = atoi(optarg);
            if (large <= 0) {
                fprintf(stderr, "Can't parse argument to -r %s\n", optarg);
                return 1;
            }
            serverOpts.defaultArguments.append("-Wlarge-by-value-copy=" + String(optarg)); // ### not quite working
            break; }
        case 'D': {
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
        case 'I':
            serverOpts.includePaths.append(Source::Include(Source::Include::Type_Include, Path::resolved(optarg)));
            break;
        case 's':
            serverOpts.includePaths.append(Source::Include(Source::Include::Type_System, Path::resolved(optarg)));
            break;
        case 'L':
            logFile = optarg;
            logLevel = LogLevel::None;
            break;
        case 'v':
            if (logLevel != LogLevel::None)
                ++logLevel;
            break;
#ifdef RTAGS_HAS_LAUNCHD
        case 4:
            serverOpts.options |= Server::Launchd;
            break;
#endif
        case 5:
            inactivityTimeout = atoi(optarg); // seconds.
            if (inactivityTimeout <= 0) {
                fprintf(stderr, "Invalid argument to --inactivity-timeout %s\n", optarg);
                return 1;
            }
            break;
        case 7:
            serverOpts.options |= Server::RPLogToSyslog;
            break;
        case 8:
            serverOpts.options |= Server::CompletionsNoFilter;
            break;
        case '?': {
            fprintf(stderr, "Run rdm --help for help\n");
            return 1; }
        }
    }
    if (optind < argCount) {
        fprintf(stderr, "rdm: unexpected option -- '%s'\n", args[optind]);
        return 1;
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
        serverOpts.excludeFilters = String(EXCLUDEFILTER_DEFAULT).split(';');

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
    if (defaultDataDir) {
        Path migration = String::format<128>("%s.rtags-file", Path::home().constData());
        if (migration.isDir()) {
            Rct::removeDirectory(serverOpts.dataDir);
            rename(migration.constData(), serverOpts.dataDir.constData());
            error() << "Migrated datadir from ~/.rtags-file ~/.rtags";
        }
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
            fprintf(stderr, "Couldn't open temp file %s for write (%d)", crashDumpTempFilePath, errno);
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
