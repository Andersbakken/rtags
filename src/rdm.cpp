#include <QCoreApplication>
#include "Thread.h"
#include "ThreadPool.h"
#include "Server.h"
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <Log.h>
#include <RTags.h>
#include "Rdm.h"
#include "Thread.h"
#include "EventLoop.h"
#include <signal.h>
#ifdef OS_Linux
#include <execinfo.h>
#include <cxxabi.h>
#endif
#ifdef OS_Mac
#include <mach-o/dyld.h>
#endif

void signalHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);
#ifdef OS_Linux
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
                Q_ASSERT(from = -1);
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
    delete Server::instance();
    _exit(1);
}

void usage(FILE *f)
{
    fprintf(f,
            "rdm [...options...]\n"
            "  --help|-h                  Display this page\n"
            "  --include-path|-I [arg]    Add additional include path to clang\n"
            "  --include|-i [arg]         Add additional include directive to clang\n"
            "  --define|-D [arg]          Add additional define directive to clang\n"
            "  --log-file|-L [arg]        Log to this file\n"
            "  --append|-A                Append to log file\n"
            "  --verbose|-v               Change verbosity, multiple -v's are allowed\n"
            "  --clean-slate|-C           Start from a clean slate\n"
            "  --datadir|-d [arg]         Use this as datadir (default ~/.rtags\n"
            "  --disable-sighandler|-s    Disable signal handler to dump stack for crashes\n"
            "  --cache-size|-c [size]     Cache size in MB (one cache per db, default 128MB)\n"
            "  --name|-n [name]           Name to use for server (default ~/.rtags/server)\n"
            "  --no-clang-includepath|-p  Don't use clang include paths by default\n"
            "  --usedashB|-B              Use -B for make instead of makelib\n"
            "  --thread-count|-j [arg]    Spawn this many threads for thread pool\n");
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
        { 0, 0, 0, 0 }
    };

    int jobs = ThreadPool::idealThreadCount();
    unsigned options = 0;
    List<ByteArray> defaultArguments;
    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    bool clearDataDir = false;
    Path datadir = RTags::rtagsDir();
    const ByteArray shortOptions = RTags::shortOptions(opts);
    int cacheSize = 128;
    bool enableSignalHandler = true;
    ByteArray name;
    forever {
        const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 'n':
            name = optarg;
            break;
        case 'h':
            usage(stdout);
            return 0;
        case 'B':
            options |= Server::UseDashB;
            break;
        case 'd':
            datadir = Path::resolved(optarg);
            break;
        case 'p':
            options |= Server::NoClangIncludePath;
            break;
        case 's':
            enableSignalHandler = false;
            break;
        case 'C':
            clearDataDir = true;
            break;
        case 'c': {
            bool ok;
            cacheSize = ByteArray(optarg, strlen(optarg)).toUInt(&ok);
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

    ThreadPool::globalInstance()->setConcurrentJobs(jobs);
    QCoreApplication app(argc, argv);
    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return false;
    }
    Server::setBaseDirectory(datadir, clearDataDir);
    if (clearDataDir) {
        warning("Removing contents of cache directory [%s]", datadir.constData());
    }

    warning("Running with %d jobs", jobs);

    EventLoop loop;
    EventLoopThread eventThread;

    Server *server = new Server;
    Server::Options serverOpts;
    serverOpts.options = options;
    serverOpts.defaultArguments = defaultArguments;
    serverOpts.cacheSizeMB = cacheSize;
    serverOpts.name = (name.isEmpty() ? ByteArray(RTags::rtagsDir() + "server") : name );
    if (!server->init(serverOpts)) {
        delete server;
        return 1;
    }

    eventThread.start();

    const int ret = app.exec();
    delete server;
    return ret;
}
