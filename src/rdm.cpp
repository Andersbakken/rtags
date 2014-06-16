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

#include <rct/EventLoop.h>
#include <rct/Log.h>
#include "RTags.h"
#include "Server.h"
#include <rct/Rct.h>
#include <rct/Thread.h>
#include <rct/ThreadPool.h>
#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#include <cxxabi.h>
#endif

static void sigSegvHandler(int signal)
{
    if (Server *server = Server::instance())
        server->stopServers();
    fprintf(stderr, "Caught signal %d\n", signal);
    // this is not really allowed in signal handlers but will mostly work
    const List<String>& trace = RTags::backtrace();
    auto it = trace.cbegin();
    while (it != trace.end()) {
        fprintf(stderr, "%s", it->constData());
        ++it;
    }
    fflush(stderr);
    _exit(1);
}

static Path socketFile;

static void sigIntHandler(int)
{
    if (Server *server = Server::instance())
        server->stopServers();
    _exit(1);
}

#define EXCLUDEFILTER_DEFAULT "*/CMakeFiles/*;*/cmake*/Modules/*;*/conftest.c*;/tmp/*"
#define DEFAULT_RP_VISITFILE_TIMEOUT 60000
#define DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT 60000
#define DEFAULT_RP_CONNECT_TIMEOUT 0 // won't time out
#define DEFAULT_COMPLETION_CACHE_SIZE 10
#define DEFAULT_MAX_CRASH_COUNT 5
#define XSTR(s) #s
#define STR(s) XSTR(s)
static size_t defaultStackSize = 0;
#ifdef NDEBUG
#define DEFAULT_SUSPEND_RP "off"
#else
#define DEFAULT_SUSPEND_RP "on"
#endif

static void usage(FILE *f)
{
    fprintf(f,
            "rdm [...options...]\n"
            "  --help|-h                                  Display this page.\n"
            "  --server|-s [arg]                          Run as server with no arg or connect to arg as server.\n"
            "  --enable-job-server|-z                     Enable job server.\n"
            "  --include-path|-I [arg]                    Add additional include path to clang.\n"
            "  --define|-D [arg]                          Add additional define directive to clang.\n"
            "  --log-file|-L [arg]                        Log to this file.\n"
            "  --append|-A                                Append to log file.\n"
            "  --verbose|-v                               Change verbosity, multiple -v's are allowed.\n"
            "  --clear-project-caches|-C                  Clear out project caches.\n"
            "  --disable-sighandler|-x                    Disable signal handler to dump stack for crashes.\n"
            "  --clang-includepath|-P                     Use clang include paths by default.\n"
            "  --no-Wall|-W                               Don't use -Wall.\n"
            "  --Wlarge-by-value-copy|-r [arg]            Use -Wlarge-by-value-copy=[arg] when invoking clang.\n"
            "  --no-spell-checking|-l                     Don't pass -fspell-checking.\n"
            "  --unlimited-error|-f                       Pass -ferror-limit=0 to clang.\n"
            "  --silent|-S                                No logging to stdout.\n"
            "  --exclude-filter|-X [arg]                  Files to exclude from rdm, default \"" EXCLUDEFILTER_DEFAULT "\".\n"
            "  --sync-threshold|-y [arg]                  Automatically sync after [arg] files indexed.\n"
            "  --no-rc|-N                                 Don't load any rc files.\n"
            "  --ignore-printf-fixits|-F                  Disregard any clang fixit that looks like it's trying to fix format for printf and friends.\n"
            "  --config|-c [arg]                          Use this file instead of ~/.rdmrc.\n"
            "  --data-dir|-d [arg]                        Use this directory to store persistent data (default ~/.rtags).\n"
            "  --socket-file|-n [arg]                     Use this file for the server socket (default ~/.rdm).\n"
            "  --tcp-port|-p [arg]                        Use this port for tcp server (default " STR(DEFAULT_RDM_TCP_PORT) ").\n"
            "  --setenv|-e [arg]                          Set this environment variable (--setenv \"foobar=1\").\n"
            "  --no-current-project|-o                    Don't restore the last current project on startup.\n"
            "  --disallow-multiple-sources|-m             With this setting different sources will be merged for each source file.\n"
            "  --separate-debug-and-release|-E            Normally rdm doesn't consider release and debug as different builds. Pass this if you want it to.\n"
            "  --unload-timer|-u [arg]                    Number of minutes to wait before unloading non-current projects (disabled by default).\n"
            "  --job-count|-j [arg]                       Spawn this many concurrent processes for indexing (default %d).\n"
            "  --no-local-compiles|-J                     Don't run rp ever. For debugging.\n"
            "  --watch-system-paths|-w                    Watch system paths for changes.\n"
            "  --rp-visit-file-timeout|-t [arg]           Timeout for rp visitfile commands in ms (0 means no timeout) (default " STR(DEFAULT_RP_VISITFILE_TIMEOUT) ").\n"
            "  --rp-indexer-message-timeout|-T [arg]      Timeout for rp indexer-message in ms (0 means no timeout) (default " STR(DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT) ").\n"
            "  --rp-connect-timeout|-O [arg]              Timeout for connection from rp to rdm in ms (0 means no timeout) (default " STR(DEFAULT_RP_CONNECT_TIMEOUT) ").\n"
#ifdef OS_Darwin
            "  --filemanager-watch|-M                     Use a file system watcher for filemanager.\n"
#else
            "  --no-filemanager-watch|-M                  Don't use a file system watcher for filemanager.\n"
#endif
            "  --suspend-rp-on-crash|-q [arg]             Suspend rp in SIGSEGV handler (default " DEFAULT_SUSPEND_RP ").\n"
            "  --no-no-unknown-warnings-option|-Y         Don't pass -Wno-unknown-warning-option\n"
            "  --ignore-compiler|-b [arg]                 Alias this compiler (Might be practical to avoid duplicated sources for things like icecc).\n"
            "  --thread-stack-size|-k [arg]               Set stack size for threadpool to this (default %zu).\n"
            "  --completion-cache-size|-i [arg]           Number of translation units to cache (default " STR(DEFAULT_COMPLETION_CACHE_SIZE) ").\n"
            "  --extra-compilers|-U [arg]                 Override additional \"known\" compilers. E.g. -U foobar;c++, foobar;c or foobar:objective-c or just foobar.\n"
            "  --max-crash-count|-K [arg]                 Number of restart attempts for a translation unit when rp crashes (default " STR(DEFAULT_MAX_CRASH_COUNT) ").\n",
            std::max(2, ThreadPool::idealThreadCount()), defaultStackSize);
}

int main(int argc, char** argv)
{
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
        { "server", optional_argument, 0, 's' },
        { "enable-job-server", no_argument, 0, 'z' },
        { "compression", required_argument, 0, 'Z' },
        { "include-path", required_argument, 0, 'I' },
        { "define", required_argument, 0, 'D' },
        { "log-file", required_argument, 0, 'L' },
        { "setenv", required_argument, 0, 'e' },
        { "no-Wall", no_argument, 0, 'W' },
        { "append", no_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "job-count", required_argument, 0, 'j' },
        { "no-local-compiles", no_argument, 0, 'J' },
        { "clean-slate", no_argument, 0, 'C' },
        { "disable-sighandler", no_argument, 0, 'x' },
        { "silent", no_argument, 0, 'S' },
        { "exclude-filter", required_argument, 0, 'X' },
        { "socket-file", required_argument, 0, 'n' },
        { "config", required_argument, 0, 'c' },
        { "no-rc", no_argument, 0, 'N' },
        { "data-dir", required_argument, 0, 'd' },
        { "ignore-printf-fixits", no_argument, 0, 'F' },
        { "unlimited-errors", no_argument, 0, 'f' },
        { "no-spell-checking", no_argument, 0, 'l' },
        { "sync-threshold", required_argument, 0, 'y' },
        { "large-by-value-copy", required_argument, 0, 'r' },
        { "disallow-multiple-sources", no_argument, 0, 'm' },
        { "unload-timer", required_argument, 0, 'u' },
        { "no-current-project", no_argument, 0, 'o' },
        { "no-no-unknown-warnings-option", no_argument, 0, 'Y' },
        { "ignore-compiler", required_argument, 0, 'b' },
        { "watch-system-paths", no_argument, 0, 'w' },
        { "rp-visit-file-timout", required_argument, 0, 't' },
        { "rp-indexer-message-timeout", required_argument, 0, 'T' },
        { "rp-connect-timeout", required_argument, 0, 'O' },
        { "thread-stack-size", required_argument, 0, 'k' },
        { "suspend-rp-on-crash", required_argument, 0, 'q' },
        { "separate-debug-and-release", no_argument, 0, 'E' },
        { "max-crash-count", required_argument, 0, 'K' },
        { "completion-cache-size", required_argument, 0, 'i' },
        { "extra-compilers", required_argument, 0, 'U' },
#ifdef OS_Darwin
        { "filemanager-watch", no_argument, 0, 'M' },
#else
        { "no-filemanager-watch", no_argument, 0, 'M' },
#endif
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


    List<String> argCopy;
    List<char*> argList;
    {
        bool norc = false;
        Path rcfile = Path::home() + ".rdmrc";
        opterr = 0;

        char *originalArgv[argc];
        memcpy(originalArgv, argv, sizeof(originalArgv));
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
    serverOpts.rpVisitFileTimeout = DEFAULT_RP_VISITFILE_TIMEOUT;
    serverOpts.rpIndexerMessageTimeout = DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT;
    serverOpts.rpConnectTimeout = DEFAULT_RP_CONNECT_TIMEOUT;
    serverOpts.options = Server::Wall|Server::SpellChecking;
    serverOpts.maxCrashCount = DEFAULT_MAX_CRASH_COUNT;
    serverOpts.completionCacheSize = DEFAULT_COMPLETION_CACHE_SIZE;
#ifdef OS_Darwin
    serverOpts.options |= Server::NoFileManagerWatch;
#endif
// #ifndef NDEBUG
//     serverOpts.options |= Server::SuspendRPOnCrash;
// #endif
    serverOpts.excludeFilters = String(EXCLUDEFILTER_DEFAULT).split(';');
    serverOpts.dataDir = String::format<128>("%s.rtags", Path::home().constData());
    serverOpts.unloadTimer = 0;

    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    bool sigHandler = false;
    assert(Path::home().endsWith('/'));
    int argCount = argList.size();
    char **args = argList.data();

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
            logLevel = -1;
            break;
        case 'X':
            serverOpts.excludeFilters += String(optarg).split(';');
            break;
        case 'U': {
            Source::Language lang = Source::NoLanguage;
            RegExp rx;
            if (char *semiColon = strchr(optarg, ';')) {
                for (int i=Source::NoLanguage + 1; i<=Source::ObjectiveCPlusPlus; ++i) {
                    const char *name = Source::languageName(static_cast<Source::Language>(i));
                    if (!strcasecmp(name, semiColon + 1)) {
                        lang = static_cast<Source::Language>(i);
                        break;
                    }
                }
                if (lang == Source::NoLanguage) {
                    fprintf(stderr, "Unknown language %s, available languages:\n", semiColon + 1);
                    for (int i=Source::NoLanguage + 1; i<=Source::ObjectiveCPlusPlus; ++i) {
                        fprintf(stderr, "  %s\n", Source::languageName(static_cast<Source::Language>(i)));
                    }
                    return 1;
                }
                rx = String(optarg, semiColon - optarg - 1);
            } else {
                rx = optarg;
                lang = Source::C;
            }
            serverOpts.extraCompilers.append(std::make_pair(rx, lang));
            break; }
        case 'E':
            serverOpts.options |= Server::SeparateDebugAndRelease;
            break;
        case 't':
            serverOpts.rpVisitFileTimeout = atoi(optarg);
            if (serverOpts.rpVisitFileTimeout < 0) {
                fprintf(stderr, "Invalid argument to -t %s\n", optarg);
                return 1;
            }
            if (!serverOpts.rpVisitFileTimeout)
                serverOpts.rpVisitFileTimeout = -1;
            break;
        case 'O':
            serverOpts.rpConnectTimeout = atoi(optarg);
            if (serverOpts.rpConnectTimeout < 0) {
                fprintf(stderr, "Invalid argument to -O %s\n", optarg);
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
        case 'n':
            serverOpts.socketFile = optarg;
            break;
        case 'd':
            serverOpts.dataDir = String::format<128>("%s", Path::resolved(optarg).constData());
            break;
        case 'h':
            usage(stdout);
            return 0;
        case 'Y':
            serverOpts.options |= Server::NoNoUnknownWarningsOption;
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
            if (!strcmp(optarg, "on") || !strcmp(optarg, "1")) {
                serverOpts.options |= Server::SuspendRPOnCrash;
            } else if (!strcmp(optarg, "off") || !strcmp(optarg, "1")) {
                serverOpts.options &= ~Server::SuspendRPOnCrash;
            } else {
                fprintf(stderr, "Invalid argument to -q. Must be on, off, 1, or 0\n");
                return 1;
            }
            break;
        case 'M':
#ifdef OS_Darwin
            serverOpts.options &= ~Server::NoFileManagerWatch;
#else
            serverOpts.options |= Server::NoFileManagerWatch;
#endif
            break;
        case 'F':
            serverOpts.options |= Server::IgnorePrintfFixits;
            break;
        case 'f':
            serverOpts.options |= Server::UnlimitedErrors;
            break;
        case 'l':
            serverOpts.options &= ~Server::SpellChecking;
            break;
        case 'W':
            serverOpts.options &= ~Server::Wall;
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
        case 'u': {
            bool ok;
            serverOpts.unloadTimer = static_cast<int>(String(optarg).toULongLong(&ok));
            if (!ok) {
                fprintf(stderr, "Invalid argument to --unload-timer %s\n", optarg);
                return 1;
            }
            break; }
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
        case 'y':
            serverOpts.syncThreshold = atoi(optarg);
            if (serverOpts.syncThreshold <= 0) {
                fprintf(stderr, "Invalid argument to -y %s\n", optarg);
                return 1;
            }
            break;
        case 'T':
            serverOpts.rpIndexerMessageTimeout = atoi(optarg);
            if (serverOpts.rpIndexerMessageTimeout <= 0) {
                fprintf(stderr, "Can't parse argument to -T %s.\n", optarg);
                return 1;
            }
            break;
        case 'J':
            serverOpts.options |= Server::NoLocalCompiles;
            break;
        case 'j':
            serverOpts.jobCount = atoi(optarg);
            if (serverOpts.jobCount < 0) {
                fprintf(stderr, "Can't parse argument to -j %s. -j must be a positive integer.\n", optarg);
                return 1;
            }
            break;
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
            serverOpts.includePaths.append(Path::resolved(optarg));
            break;
        case 'A':
            logFlags |= Log::Append;
            break;
        case 'L':
            logFile = optarg;
            break;
        case 'v':
            if (logLevel >= 0)
                ++logLevel;
            break;
        case '?': {
            fprintf(stderr, "Run rc --help for help\n");
            return 1; }
        }
    }
    if (optind < argCount) {
        fprintf(stderr, "rdm: unexpected option -- '%s'\n", args[optind]);
        return 1;
    }

    signal(SIGINT, sigIntHandler);
    if (sigHandler)
        signal(SIGSEGV, sigSegvHandler);

    if (!initLogging(argv[0], LogStderr, logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return 1;
    }

    EventLoop::SharedPtr loop(new EventLoop);
    loop->init(EventLoop::MainEventLoop);

    std::shared_ptr<Server> server(new Server);
    ::socketFile = serverOpts.socketFile;
    if (!serverOpts.dataDir.endsWith('/'))
        serverOpts.dataDir.append('/');
    if (!server->init(serverOpts)) {
        cleanupLogging();
        return 1;
    }

    loop->exec();
    cleanupLogging();
    return 0;
}
