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
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#include <cxxabi.h>
#endif

void signalHandler(int signal)
{
    extern bool inSignalHandler;
    inSignalHandler = true;
    fprintf(stderr, "Caught signal %d\n", signal);
    ByteArray trace = RTags::backtrace();
    fprintf(stderr, "%s", trace.constData());
#ifdef HAVE_BACKTRACE
    enum { StackSize = 50 };
    void *callstack[StackSize];
    const int c = backtrace(callstack, StackSize);
    char **symbols = backtrace_symbols(callstack, c);
    for (int i=0; i<c; ++i) {
        const char *frame = symbols[i];
        int from = -1;
        int to = -1;
        for (int j=0; frame[j]; ++j) {
            switch (frame[j]) {
            case '(':
                assert(from == -1);
                from = j;
                break;
            case '+':
                if (from != -1) {
                    to = j;
                    break;
                }
                break;
            }
        }
        if (from != -1 && to != -1) {
            char buf[1024];
            size_t len = sizeof(buf);
            assert(to - from < (int)len);
            memcpy(buf, frame + from + 1, to - from - 1);
            buf[to - from - 1] = '\0';
            int status;
            abi::__cxa_demangle(buf, buf, &len, &status);
            if (!status) {
                fprintf(stderr, "  %d/%d %s [%p]\n", i + 1, c, buf, callstack[i]);
                continue;
            }
        }
        fprintf(stderr, "  %d/%d %s\n", i + 1, c, frame);
    }
    free(symbols);
#endif
    fflush(stderr);
    EventLoop::instance()->exit();
}

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
            "  --datadir|-d [arg]              Use this as datadir (default ~/.rtags\n"
            "  --disable-sighandler|-s         Disable signal handler to dump stack for crashes\n"
            "  --cache-size|-c [size]          Cache size in MB (one cache per db, default 128MB)\n"
            "  --name|-n [name]                Name to use for server (default ~/.rtags/server)\n"
            "  --no-clang-includepath|-p       Don't use clang include paths by default\n"
            "  --usedashB|-B                   Use -B for make instead of makelib\n"
            "  --silent|-S                     No logging to stdout\n"
            "  --max-completion-units|-x [arg] Max translation units to keep in memory for completions (default 10)\n"
            "  --no-validate-on-startup|-V     Disable validation of database on startup\n"
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
        { "no-clang-includepath", no_argument, 0, 'p' },
        { "append", no_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "thread-count", required_argument, 0, 'j' },
        { "datadir", required_argument, 0, 'd' },
        { "clean-slate", no_argument, 0, 'C' },
        { "cache-size", required_argument, 0, 'c' },
        { "disable-sighandler", no_argument, 0, 's' },
        { "name", required_argument, 0, 'n' },
        { "usedashB", no_argument, 0, 'B' },
        { "silent", no_argument, 0, 'S' },
        { "max-completion-units", required_argument, 0, 'x' },
        { "no-validate-on-startup", no_argument, 0, 'V' },
        { 0, 0, 0, 0 }
    };

    int jobs = ThreadPool::idealThreadCount();
    unsigned options = 0;
    List<ByteArray> defaultArguments;
    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    Path dataDir = RTags::rtagsDir();
    const ByteArray shortOptions = RTags::shortOptions(opts);
    int cacheSize = 128;
    int maxCompletionUnits = 10;
    bool enableSignalHandler = true;
    ByteArray name;
    while (true) {
        const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 'S':
            logLevel = -1;
            break;
        case 'x': {
            const ByteArray arg(optarg);
            bool ok;
            maxCompletionUnits = arg.toULongLong(&ok);
            if (!ok) {
                fprintf(stderr, "%s is not a valid argument for -x\n", optarg);
                return 1;
            }
            break; }
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
            options |= Server::NoValidateOnStartup;
            break;
        case 'd':
            dataDir = Path::resolved(optarg);
            break;
        case 'p':
            options |= Server::NoClangIncludePath;
            break;
        case 's':
            enableSignalHandler = false;
            break;
        case 'C':
            options |= Server::ClearDatadir;
            break;
        case 'c': {
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
    if (optind < argc) {
        fprintf(stderr, "rdm: unexpected option -- '%s'\n", argv[optind]);
        return 1;
    }

    if (enableSignalHandler) {
        signal(SIGINT, signalHandler);
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
    serverOpts.path = dataDir;
    if (!serverOpts.path.endsWith('/'))
        serverOpts.path.append('/');
    serverOpts.maxCompletionUnits = maxCompletionUnits;
    serverOpts.options = options;
    serverOpts.defaultArguments = defaultArguments;
    serverOpts.cacheSizeMB = cacheSize;
    serverOpts.threadCount = jobs;
    serverOpts.socketPath = (name.isEmpty() ? ByteArray(RTags::rtagsDir() + "server") : name );
    if (!server->init(serverOpts)) {
        delete server;
        return 1;
    }

    loop.run();
    delete server;
    return 0;
}
