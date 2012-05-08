#include <QCoreApplication>
#include <QThread>
#include <QThreadPool>
#include "Server.h"
#include <getopt.h>
#include <QDateTime>
#include <stdio.h>
#include <stdlib.h>
#include <Log.h>
#include <RTags.h>
#include "Rdm.h"
#include <signal.h>
#include <execinfo.h>

void signalHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);
    enum { StackSize = 50 };
    void *callstack[StackSize];
    const int c = backtrace(callstack, StackSize);
    char **symbols = backtrace_symbols(callstack, c);
    for (int i = 0; i < c; ++i)
        fprintf(stderr, "  %d/%d %p %s\n", i + 1, c, callstack[i], symbols[i]);
    free(symbols);
    fflush(stderr);
    delete Server::instance();
    _exit(1);
}

void usage(FILE *f)
{
    fprintf(f,
            "rdm ...options...\n"
            "  --help|-h                  Display this page\n"
            "  --include-path|-I [arg]    Add additional include path to clang\n"
            "  --include|-i [arg]         Add additional include directive to clang\n"
            "  --define|-D [arg]          Add additional define directive to clang\n"
            "  --log-file|-L [arg]        Log to this file\n"
            "  --append|-A                Append to log file\n"
            "  --verbose|-v               Change verbosity, multiple -v's are allowed\n"
            "  --clean-slate|-C           Start from a clean slate\n"
            "  --datadir|-d [arg]         Use this as datadir (default ~/.rtags\n"
            "  --enable-sighandler|-s     Enable signal handler to dump stack for crashes\n"
            "  --cache-size|-c [size]     Cache size in MB (one cache per db, default 128MB)\n"
            "  --max-memory-use|-M [size] Max amount of memory to use in MB default 1024MB\n"
            "  --thread-count|-j [arg]    Spawn this many threads for thread pool\n");
}

int main(int argc, char** argv)
{
    struct option opts[] = {
        { "help", no_argument, 0, 'h' },
        { "include-path", required_argument, 0, 'I' },
        { "include", required_argument, 0, 'i' },
        { "define", required_argument, 0, 'D' },
        { "log-file", required_argument, 0, 'L' },
        { "append", no_argument, 0, 'A' },
        { "verbose", no_argument, 0, 'v' },
        { "thread-count", required_argument, 0, 'j' },
        { "datadir", required_argument, 0, 'd' },
        { "clean-slate", no_argument, 0, 'C' },
        { "cache-size", required_argument, 0, 'c' },
        { "max-memory-use", required_argument, 0, 'M' },
        { "enable-sighandler", no_argument, 0, 's' },
        { 0, 0, 0, 0 }
    };

    int jobs = QThread::idealThreadCount();
    unsigned options = 0;
    QList<QByteArray> defaultArguments;
    const char *logFile = 0;
    unsigned logFlags = 0;
    int logLevel = 0;
    bool clearDataDir = false;
    Path datadir = (QDir::homePath() + "/.rtags/").toLocal8Bit();
    const QByteArray shortOptions = RTags::shortOptions(opts);
    int cacheSize = 128;
    long maxMemoryUse = 1024;

    forever {
        const int c = getopt_long(argc, argv, shortOptions.constData(), opts, 0);
        if (c == -1)
            break;
        switch (c) {
        case 'h':
            usage(stdout);
            return 0;
        case 'd':
            datadir = Path::resolved(optarg);
            break;
        case 's':
            signal(SIGINT, signalHandler);
            break;
        case 'C':
            clearDataDir = true;
            break;
        case 'c': {
            bool ok;
            cacheSize = QByteArray::fromRawData(optarg, strlen(optarg)).toUInt(&ok);
            if (!ok) {
                fprintf(stderr, "Can't parse argument to -c %s\n", optarg);
                return 1;
            }
            break; }
        case 'M':
            maxMemoryUse = atoi(optarg);
            if (maxMemoryUse <= 0) {
                fprintf(stderr, "Can't parse argument to -M %s\n", optarg);
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
            defaultArguments.append("-D" + QByteArray(optarg));
            break;
        case 'I':
            defaultArguments.append("-I" + QByteArray(optarg));
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

    QThreadPool::globalInstance()->setMaxThreadCount(jobs);
    QCoreApplication app(argc, argv);
    if (!initLogging(logLevel, logFile, logFlags)) {
        fprintf(stderr, "Can't initialize logging with %d %s 0x%0x\n",
                logLevel, logFile ? logFile : "", logFlags);
        return false;
    }
    Rdm::setMaxMemoryUsage(maxMemoryUse * 1024 * 1024);
    Server::setBaseDirectory(datadir, clearDataDir);
    if (clearDataDir) {
        warning("Removing contents of cache directory [%s]", datadir.constData());
    }

    warning("Running with %d jobs", jobs);

    Server *server = new Server;
    const Server::Options serverOpts = { options, defaultArguments, cacheSize };
    if (!server->init(serverOpts)) {
        delete server;
        return 1;
    }

    const int ret = app.exec();
    delete server;
    return ret;
}
