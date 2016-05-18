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

#define RTAGS_SINGLE_THREAD
#include <signal.h>
#include <syslog.h>

#include "ClangIndexer.h"
#include "Project.h"
#include "rct/Log.h"
#include "rct/StopWatch.h"
#include "rct/String.h"
#include "RTags.h"
#include "Server.h"
#include "Source.h"

static void sigHandler(int signal)
{
    // this is not really allowed in signal handlers but will mostly work
    const String trace = Rct::backtrace();
    if (ClangIndexer::serverOpts() & Server::SuspendRPOnCrash) {
        int seconds = 2;
        printf("@CRASH@Caught signal %d\n%s@CRASH@", signal, trace.constData());
        while (true) {
            printf("@CRASH@rp crashed, waiting for debugger pid: %d@CRASH@", getpid());
            fflush(stdout);
            sleep(seconds);
            if (seconds < 32)
                seconds *= 2;
        }
    }
    fprintf(stderr, "Caught signal %d\n%s\n", signal, trace.constData());
    fflush(stderr);
    ::closelog();
    _exit(1);
}

struct SyslogCloser
{
public:
    ~SyslogCloser()
    {
        ::closelog();
    }
};

int main(int argc, char **argv)
{
    LogLevel logLevel = LogLevel::Error;
    Path file;
    String sbroot;

    for (int i=1; i<argc; ++i) {
        if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbose")) {
            ++logLevel;
        } else if (!strcmp(argv[i], "--priority")) { // ignore, only for wrapping purposes
            ++i;
        } else if (!strcmp(argv[i], "--sandbox-root")) { 
            // SBROOT
            ++i;
            sbroot = argv[i];
            ++i;
        } else {
            file = argv[i];
        }
    } 
    
    // SBROOT
    if (ClangIndexer::serverSandboxRoot().isEmpty()) {
        ClangIndexer::setServerSandboxRoot(sbroot);
    }

    setenv("LIBCLANG_NOTHREADS", "1", 0);
    signal(SIGSEGV, sigHandler);
    signal(SIGABRT, sigHandler);
    signal(SIGBUS, sigHandler);

    Flags<LogFlag> logFlags = LogStderr;
    std::shared_ptr<SyslogCloser> closer;
    if (ClangIndexer::serverOpts() & Server::RPLogToSyslog) {
        logFlags |= LogSyslog;
        closer.reset(new SyslogCloser);
    }
    initLogging(argv[0], logFlags, logLevel);
    (void)closer;

    RTags::initMessages();
    std::shared_ptr<EventLoop> eventLoop(new EventLoop);
    eventLoop->init(EventLoop::MainEventLoop);
    String data;

    if (!file.isEmpty()) {
        data = file.readAll();
    } else {
        uint32_t size;
        if (!fread(&size, sizeof(size), 1, stdin)) {
            error() << "Failed to read from stdin";
            return 1;
        }
        data.resize(size);
        if (!fread(&data[0], size, 1, stdin)) {
            error() << "Failed to read from stdin";
            return 2;
        }
        // FILE *f = fopen("/tmp/data", "w");
        // fwrite(data.constData(), data.size(), 1, f);
        // fclose(f);
    }
    ClangIndexer indexer;
    if (!indexer.exec(data)) {
        error() << "ClangIndexer error";
        return 3;
    }

    return 0;
}
