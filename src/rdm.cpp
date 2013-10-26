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
#include "ScanJob.h"
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#include <cxxabi.h>
#endif

static void sigSegvHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);
    String trace = RTags::backtrace();
    if (!trace.isEmpty()) {
        fprintf(stderr, "%s", trace.constData());
    }
    fflush(stderr);
    _exit(1);
}

static Path socketFile;

static void sigIntHandler(int)
{
    unlink(socketFile.constData());
    _exit(1);
}

#define EXCLUDEFILTER_DEFAULT "*/CMakeFiles/*;*/cmake*/Modules/*;*/conftest.c*;/tmp/*"
#define DEFAULT_COMPLETION_CACHE_CLEAR_INTERVAL 60
#define DEFAULT_RP_VISITFILE_TIMEOUT 3000
#define DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT 5000
#define XSTR(s) #s
#define STR(s) XSTR(s)

static void usage(FILE *f)
{
    fprintf(f,
            "rdm [...options...]\n"
            "  --help|-h                                  Display this page.\n"
            "  --include-path|-I [arg]                    Add additional include path to clang.\n"
            "  --define|-D [arg]                          Add additional define directive to clang.\n"
            "  --log-file|-L [arg]                        Log to this file.\n"
            "  --append|-A                                Append to log file.\n"
            "  --verbose|-v                               Change verbosity, multiple -v's are allowed.\n"
            "  --clear-project-caches|-C                  Clear out project caches.\n"
            "  --enable-sighandler|-s                     Enable signal handler to dump stack for crashes..\n"
            "                                             Note that this might not play well with clang's signal handler.\n"
            "  --clang-includepath|-P                     Use clang include paths by default.\n"
            "  --no-Wall|-W                               Don't use -Wall.\n"
            "  --Wlarge-by-value-copy|-r [arg]            Use -Wlarge-by-value-copy=[arg] when invoking clang.\n"
            "  --no-spell-checking|-l                     Don't pass -fspell-checking.\n"
            "  --unlimited-error|-f                       Pass -ferror-limit=0 to clang.\n"
            "  --silent|-S                                No logging to stdout.\n"
            "  --validate|-V                              Enable validation of database on startup and after indexing.\n"
            "  --exclude-filter|-x [arg]                  Files to exclude from rdm, default \"" EXCLUDEFILTER_DEFAULT "\".\n"
            "  --sync-threshold|-y [arg]                  Automatically sync after [arg] files indexed\n"
            "  --no-rc|-N                                 Don't load any rc files.\n"
            "  --ignore-printf-fixits|-F                  Disregard any clang fixit that looks like it's trying to fix format for printf and friends.\n"
            "  --config|-c [arg]                          Use this file instead of ~/.rdmrc.\n"
            "  --data-dir|-d [arg]                        Use this directory to store persistent data (default ~/.rtags).\n"
            "  --socket-file|-n [arg]                     Use this file for the server socket (default ~/.rdm).\n"
            "  --setenv|-e [arg]                          Set this environment variable (--setenv \"foobar=1\").\n"
            "  --completion-cache-size|-a [arg]           Cache this many translation units (default 0, must have at least 1 to use completion).\n"
            "  --no-current-project|-o                    Don't restore the last current project on startup.\n"
            "  --allow-multiple-sources|-m                 Without this setting different sources will be merged for each source file.\n"
            "  --unload-timer|-u [arg]                    Number of minutes to wait before unloading non-current projects (disabled by default).\n"
            "  --job-count|-j [arg]                       Spawn this many concurrent processes for indexing.\n"
            "  --watch-system-paths|-w                    Watch system paths for changes.\n"
            "  --clear-completion-cache-interval|-O [arg] Set completion cache cleanup interval in minuts. (default " STR(DEFAULT_COMPLETION_CACHE_CLEAR_INTERVAL) ")\n"
            "  --rp-visit-file-timeout|-t [arg]           Timeout for rp visitfile commands in ms (0 means no timeout) (default " STR(DEFAULT_RP_VISITFILE_TIMEOUT) ")\n"
            "  --rp-indexer-message-timeout|-T [arg]      Timeout for rp indexer-message in ms (0 means no timeout) (default " STR(DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT) ")\n"

#ifdef OS_Darwin
            "  --filemanager-watch|-M                     Use a file system watcher for filemanager.\n"
#else
            "  --no-filemanager-watch|-M                  Don't use a file system watcher for filemanager.\n"
#endif
            "  --ignore-compiler|-b [arg]                 Alias this compiler (Might be practical to avoid duplicated sources for things like icecc).\n"
            "  --disable-plugin|-p [arg]                  Don't load this plugin\n"
            "  --disable-esprima|-E                       Don't use esprima\n"
            "  --enable-compiler-flags|-K                 Query the compiler for default flags\n");
}

int main(int argc, char** argv)
{
    Rct::findExecutablePath(*argv);

    struct option opts[] = {
        { "help", no_argument, 0, 'h' },
        { "include-path", required_argument, 0, 'I' },
        { "define", required_argument, 0, 'D' },
        { "log-file", required_argument, 0, 'L' },
        { "no-builtin-includes", no_argument, 0, 'U' },
        { "setenv", required_argument, 0, 'e' },
        { "no-Wall", no_argument, 0, 'W' },
        { "append", no_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "thread-count", required_argument, 0, 'j' },
        { "clean-slate", no_argument, 0, 'C' },
        { "enable-sighandler", no_argument, 0, 's' },
        { "silent", no_argument, 0, 'S' },
        { "validate", no_argument, 0, 'V' },
        { "exclude-filter", required_argument, 0, 'x' },
        { "socket-file", required_argument, 0, 'n' },
        { "config", required_argument, 0, 'c' },
        { "no-rc", no_argument, 0, 'N' },
        { "data-dir", required_argument, 0, 'd' },
        { "ignore-printf-fixits", no_argument, 0, 'F' },
        { "unlimited-errors", no_argument, 0, 'f' },
        { "completion-cache-size", required_argument, 0, 'a' },
        { "no-spell-checking", no_argument, 0, 'l' },
        { "sync-threshold", required_argument, 0, 'y' },
        { "large-by-value-copy", required_argument, 0, 'r' },
        { "allow-multiple-sources", no_argument, 0, 'm' },
        { "unload-timer", required_argument, 0, 'u' },
        { "no-current-project", no_argument, 0, 'o' },
        { "ignore-compiler", required_argument, 0, 'b' },
        { "disable-plugin", required_argument, 0, 'p' },
        { "watch-system-paths", no_argument, 0, 'w' },
        { "disable-esprima", no_argument, 0, 'E' },
        { "enable-compiler-flags", no_argument, 0, 'K' },
        { "clear-completion-cache-interval", required_argument, 0, 'O' },
        { "rp-visit-file-timout", required_argument, 0, 't' },
        { "rp-indexer-message-timeout", required_argument, 0, 'T' },
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
            char *rc;
            int size = Path("/etc/rdmrc").readAll(rc);
            if (rc) {
                argCopy = String(rc, size).split('\n');
                delete[] rc;
            }
            if (!rcfile.isEmpty()) {
                size = rcfile.readAll(rc);
                if (rc) {
                    List<String> split = String(rc, size).split('\n');
                    argCopy.append(split);
                    delete[] rc;
                }
            }
            const int s = argCopy.size();
            for (int i=0; i<s; ++i) {
                String &arg = argCopy.at(i);
                if (!arg.isEmpty() && !arg.startsWith('#') && !arg.startsWith(' '))
                    argList.append(arg.data());
            }
        }
        for (int i=1; i<argc; ++i) {
            argList.append(argv[i]);
        }

        optind = 1;
    }

    Server::Options serverOpts;
    serverOpts.socketFile = String::format<128>("%s.rdm", Path::home().constData());
    serverOpts.processCount = ThreadPool::idealThreadCount();
    serverOpts.completionCacheSize = 0;
    serverOpts.clearCompletionCacheInterval = DEFAULT_COMPLETION_CACHE_CLEAR_INTERVAL;
    serverOpts.rpVisitFileTimeout = DEFAULT_RP_VISITFILE_TIMEOUT;
    serverOpts.rpIndexerMessageTimeout = DEFAULT_RP_INDEXER_MESSAGE_TIMEOUT;
    serverOpts.options = Server::Wall|Server::SpellChecking;
#ifdef OS_Darwin
    serverOpts.options |= Server::NoFileManagerWatch;
#endif
    serverOpts.excludeFilters = String(EXCLUDEFILTER_DEFAULT).split(';');
    serverOpts.dataDir = String::format<128>("%s.rtags", Path::home().constData());
    serverOpts.unloadTimer = 0;

    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
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
        case 'x':
            serverOpts.excludeFilters += String(optarg).split(';');
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
        case 'K':
            serverOpts.options |= Server::UseCompilerFlags;
            break;
        case 'E':
            serverOpts.options |= Server::NoEsprima;
            break;
        case 'm':
            serverOpts.options |= Server::AllowMultipleSources;
            break;
        case 'V':
            serverOpts.options |= Server::Validate;
            break;
        case 'o':
            serverOpts.options |= Server::NoStartupCurrentProject;
            break;
        case 'w':
            serverOpts.options |= Server::WatchSystemPaths;
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
        case 'U':
            serverOpts.options |= Server::NoBuiltinIncludes;
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
        case 's':
            signal(SIGSEGV, sigSegvHandler);
            break;
        case 'u': {
            bool ok;
            serverOpts.unloadTimer = static_cast<int>(String(optarg).toULongLong(&ok));
            if (!ok) {
                fprintf(stderr, "Invalid argument to --unload-timer %s\n", optarg);
                return 1;
            }
            break; }
        case 'y':
            serverOpts.syncThreshold = atoi(optarg);
            if (serverOpts.syncThreshold <= 0) {
                fprintf(stderr, "Invalid argument to -y %s\n", optarg);
                return 1;
            }
            break;
        case 'a':
            serverOpts.completionCacheSize = atoi(optarg);
            if (serverOpts.completionCacheSize < 1) {
                fprintf(stderr, "Invalid argument to -a %s\n", optarg);
                return 1;
            }
            break;
        case 'O': {
            bool ok;
            serverOpts.clearCompletionCacheInterval = String(optarg).toULongLong(&ok);
            if (!ok) {
                fprintf(stderr, "Invalid argument to -O %s\n", optarg);
                return 1;
            }
            break; }
        case 'j':
            serverOpts.processCount = atoi(optarg);
            if (serverOpts.processCount <= 0) {
                fprintf(stderr, "Can't parse argument to -j %s\n", optarg);
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
        case '?':
            usage(stderr);
            return 1;
        }
    }
    if (optind < argCount) {
        fprintf(stderr, "rdm: unexpected option -- '%s'\n", args[optind]);
        return 1;
    }

    signal(SIGINT, sigIntHandler);

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return 1;
    }
    warning("Running with %d jobs", serverOpts.processCount);

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

    const unsigned int ret = loop->exec();
    cleanupLogging();
    return ret == EventLoop::Success ? 0 : 1;
}
