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
#include "GRScanJob.h"
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


void sigIntHandler(int signal)
{
    EventLoop::instance()->exit();
}

#define EXCLUDEFILTER_DEFAULT "*.o;*.a;*.so*;*.obj;*.lo;*.git/objects*"
void usage(FILE *f)
{
    fprintf(f,
            "rdm [...options...]\n"
            "  --help|-h                       Display this page\n"
            "  --include-path|-I [arg]         Add additional include path to clang\n"
            "  --include|-i [arg]              Add additional include directive to clang\n"
            "  --define|-D [arg]               Add additional define directive to clang\n"
            "  --log-file|-L [arg]             Log to this file\n"
            "  --append|-A                     Append to log file\n"
            "  --verbose|-v                    Change verbosity, multiple -v's are allowed\n"
            "  --clean-slate|-C                Start from a clean slate\n"
            "  --disable-sighandler|-s         Disable signal handler to dump stack for crashes\n"
            "  --cache-size|-a [size]          Cache size in MB (one cache per db, default 128MB)\n"
            "  --name|-n [name]                Name to use for server (default ~/.rtags/server)\n"
            "  --no-clang-includepath|-P       Don't use clang include paths by default\n"
            "  --usedashB|-B                   Use -B for make instead of makelib\n"
            "  --silent|-S                     No logging to stdout\n"
            "  --no-validate|-V                Disable validation of database on startup and after indexing\n"
            "  --exclude-filter|-x [arg]       Files to exclude from grtags, default \"" EXCLUDEFILTER_DEFAULT "\"\n"
            "  --no-rc|-N                      Don't load any rc files\n"
            "  --rc-file|-c [arg]              Use this file instead of ~/.rdmrc\n"
            "  --projects-file|-p [arg]        Use this file as a projects file (default ~/.rtagsprojects)\n"
            "  --socket-file|-o                Use this file for the server socket (default ~/.rdm)\n"
            "  --thread-count|-j [arg]         Spawn this many threads for thread pool\n");
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
        { "append", no_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "thread-count", required_argument, 0, 'j' },
        { "clean-slate", no_argument, 0, 'C' },
        { "cache-size", required_argument, 0, 'a' },
        { "disable-sighandler", no_argument, 0, 's' },
        { "name", required_argument, 0, 'n' },
        { "usedashB", no_argument, 0, 'B' },
        { "silent", no_argument, 0, 'S' },
        { "no-validate", no_argument, 0, 'V' },
        { "exclude-filter", required_argument, 0, 'x' },
        { "socket-file", required_argument, 0, 'o' },
        { "projects-file", required_argument, 0, 'p' },
        { "rc-file", required_argument, 0, 'c' },
        { "no-rc", no_argument, 0, 'N' },
        { 0, 0, 0, 0 }
    };
    const ByteArray shortOptions = RTags::shortOptions(opts);

    List<ByteArray> argCopy;
    List<char*> argList;
    {
        bool norc = false;
        Path rcfile = Path::home() + "/.rdmrc";
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
    unsigned options = 0;
    List<ByteArray> defaultArguments;
    const char *excludeFilter = 0;
    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    Path projectsFile = Path::home() + "/.rtagsprojects";
    Path socketFile = Path::home() + "/.rdm";
    int cacheSize = 128;
    bool enableSignalHandler = true;
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
        case 'n':
            name = optarg;
            break;
        case 'h':
            usage(stdout);
            return 0;
        case 'B':
            options |= Server::UseDashB;
            break;
        case 'V':
            options |= Server::NoValidate;
            break;
        case 'p':
            projectsFile = Path::resolved(optarg);
            break;
        case 'o':
            socketFile = Path::resolved(optarg);
            break;
        case 'P':
            options |= Server::NoClangIncludePath;
            break;
        case 's':
            enableSignalHandler = false;
            break;
        case 'C':
            options |= Server::ClearProjects;
            break;
        case 'a': {
            bool ok;
            cacheSize = ByteArray(optarg, strlen(optarg)).toULongLong(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -c %s\n", optarg);
                return 1;
            }
            break; }
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
            logFlags |= Append;
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

    if (enableSignalHandler) {
        signal(SIGINT, sigIntHandler);
        signal(SIGSEGV, sigSegvHandler);
    }

    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return 1;
    }
    warning("Running with %d jobs", jobs);

    EventLoop loop;

    Server *server = new Server;
    Server::Options serverOpts;
    serverOpts.excludeFilter = ByteArray(excludeFilter ? excludeFilter : EXCLUDEFILTER_DEFAULT).split(';');
    serverOpts.socketFile = socketFile;
    serverOpts.options = options;
    serverOpts.defaultArguments = defaultArguments;
    serverOpts.cacheSizeMB = cacheSize;
    serverOpts.threadCount = jobs;
    serverOpts.projectsFile = projectsFile;
    if (!server->init(serverOpts)) {
        delete server;
        cleanupLogging();
        return 1;
    }

    loop.run();
    delete server;
    cleanupLogging();
    return 0;
}
