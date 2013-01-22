#include "EventLoop.h"
#include "Log.h"
#include "RTags.h"
#include "Server.h"
#include "Thread.h"
#include "Thread.h"
#include "ThreadPool.h"
#include "config.h"
#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include "ScanJob.h"
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#include <cxxabi.h>
#endif

void sigSegvHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);
    ByteArray trace = RTags::backtrace();
    if (!trace.isEmpty()) {
        fprintf(stderr, "%s", trace.constData());
    }
    fflush(stderr);
    _exit(1);
}

static Path socketFile;

void sigIntHandler(int)
{
    unlink(socketFile.constData());
    _exit(1);
}

#define EXCLUDEFILTER_DEFAULT "*/CMakeFiles/*;*/cmake*/Modules/*"
void usage(FILE *f)
{
    fprintf(f,
            "rdm [...options...]\n"
            "  --help|-h                         Display this page\n"
            "  --include-path|-I [arg]           Add additional include path to clang\n"
            "  --include|-i [arg]                Add additional include directive to clang\n"
            "  --define|-D [arg]                 Add additional define directive to clang\n"
            "  --log-file|-L [arg]               Log to this file\n"
            "  --append|-A                       Append to log file\n"
            "  --verbose|-v                      Change verbosity, multiple -v's are allowed\n"
            "  --clear-project-caches|-C         Clear out project caches\n"
            "  --enable-sighandler|-s            Enable signal handler to dump stack for crashes.\n"
            "                                    Note that this might not play well with clang's signal handler\n"
            "  --clang-includepath|-P            Use clang include paths by default\n"
            "  --no-Wall|-W                      Don't use -Wall\n"
            "  --silent|-S                       No logging to stdout\n"
            "  --validate|-V                     Enable validation of database on startup and after indexing\n"
            "  --exclude-filter|-x [arg]         Files to exclude from rdm, default \"" EXCLUDEFILTER_DEFAULT "\"\n"
            "  --no-rc|-N                        Don't load any rc files\n"
            "  --ignore-printf-fixits|-F         Disregard any clang fixit that looks like it's trying to fix format for printf and friends\n"
            "  --rc-file|-c [arg]                Use this file instead of ~/.rdmrc\n"
            "  --projects-file|-p [arg]          Use this file as a projects file (default ~/.rtagsprojects)\n"
            "  --data-dir|-d [arg]               Use this directory to store persistent data (default ~/.rtags)\n"
            "  --socket-file|-n [arg]            Use this file for the server socket (default ~/.rdm)\n"
            "  --setenv|-e [arg]                 Set this environment variable (--setenv \"foobar=1\")\n"
            "  --completion-cache-size|-a [arg]  Cache this many translation units (default 10, min 1)\n"
            "  --no-unlimited-error|-f           Don't pass -ferror-limit=0 to clang\n"
            "  --thread-count|-j [arg]           Spawn this many threads for thread pool\n");
}

int main(int argc, char** argv)
{
    RTags::findApplicationDirPath(*argv);

    struct option opts[] = {
        { "help", no_argument, 0, 'h' },
        { "include-path", required_argument, 0, 'I' },
        { "include", required_argument, 0, 'i' },
        { "define", required_argument, 0, 'D' },
        { "log-file", required_argument, 0, 'L' },
        { "no-clang-includepath", no_argument, 0, 'P' },
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
        { "projects-file", required_argument, 0, 'p' },
        { "rc-file", required_argument, 0, 'c' },
        { "no-rc", no_argument, 0, 'N' },
        { "data-dir", required_argument, 0, 'd' },
        { "ignore-printf-fixits", no_argument, 0, 'F' },
        { "unlimited-errors", no_argument, 0, 'f' },
        { "completion-cache-size", required_argument, 0, 'a' },
        { 0, 0, 0, 0 }
    };
    const ByteArray shortOptions = RTags::shortOptions(opts);

    List<ByteArray> argCopy;
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
                argCopy = ByteArray(rc, size).split('\n');
                delete[] rc;
            }
            if (!rcfile.isEmpty()) {
                size = rcfile.readAll(rc);
                if (rc) {
                    List<ByteArray> split = ByteArray(rc, size).split('\n');
                    argCopy.append(split);
                    delete[] rc;
                }
            }
            const int s = argCopy.size();
            for (int i=0; i<s; ++i) {
                ByteArray &arg = argCopy.at(i);
                if (!arg.isEmpty() && !arg.startsWith('#') && !arg.startsWith(' '))
                    argList.append(arg.data());
            }
        }
        for (int i=1; i<argc; ++i) {
            argList.append(argv[i]);
        }

        optind = 1;
    }

    int jobs = ThreadPool::idealThreadCount();
    int completionCacheSize = 10;
    unsigned options = 0;
    List<ByteArray> defaultArguments;
    ByteArray excludeFilters = EXCLUDEFILTER_DEFAULT;
    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    assert(Path::home().endsWith('/'));
    Path projectsFile = ByteArray::format<128>("%s.rtagsprojects", Path::home().constData());
    Path dataDir = ByteArray::format<128>("%s.rtags", Path::home().constData());
    socketFile = ByteArray::format<128>("%s.rdm", Path::home().constData());
    bool enableSignalHandler = false;
    ByteArray name;
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
            if (!excludeFilters.isEmpty())
                excludeFilters += ';';
            excludeFilters += optarg;
            break;
        case 'n':
            socketFile = optarg;
            break;
        case 'd':
            dataDir = ByteArray::format<128>("%s", Path::resolved(optarg).constData());
            break;
        case 'h':
            usage(stdout);
            return 0;
        case 'V':
            options |= Server::Validate;
            break;
        case 'F':
            options |= Server::IgnorePrintfFixits;
            break;
        case 'f':
            options |= Server::UnlimitedErrors;
            break;
        case 'e':
            putenv(optarg);
            break;
        case 'p':
            projectsFile = Path::resolved(optarg);
            break;
        case 'P':
            options |= Server::ClangIncludePath;
            break;
        case 'W':
            options |= Server::NoWall;
            break;
        case 's':
            enableSignalHandler = true;
            break;
        case 'C':
            options |= Server::ClearProjects;
            break;
        case 'a':
            completionCacheSize = atoi(optarg);
            if (completionCacheSize < 1) {
                fprintf(stderr, "Invalid argument to -a %s\n", optarg);
                return 1;
            }
            break;
        case 'j':
            jobs = atoi(optarg);
            if (jobs <= 0) {
                fprintf(stderr, "Can't parse argument to -j %s\n", optarg);
                return 1;
            }
            break;
        case 'D':
            defaultArguments.append("-D" + ByteArray(optarg));
            break;
        case 'I':
            defaultArguments.append("-I" + ByteArray(optarg));
            break;
        case 'i':
            defaultArguments.append("-include");
            defaultArguments.append(optarg);
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

    if (enableSignalHandler)
        signal(SIGSEGV, sigSegvHandler);
    signal(SIGINT, sigIntHandler);

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return 1;
    }
    warning("Running with %d jobs", jobs);

    EventLoop loop;

    Server server;
    Server::Options serverOpts;
    serverOpts.socketFile = socketFile;
    serverOpts.options = options;
    serverOpts.dataDir = dataDir;
    serverOpts.excludeFilters = excludeFilters.split(';');
    serverOpts.completionCacheSize = completionCacheSize;
    if (!serverOpts.dataDir.endsWith('/'))
        serverOpts.dataDir.append('/');
    serverOpts.defaultArguments = defaultArguments;
    serverOpts.threadCount = jobs;
    serverOpts.projectsFile = projectsFile;
    if (!server.init(serverOpts)) {
        cleanupLogging();
        return 1;
    }

    loop.run();
    cleanupLogging();
    return 0;
}
