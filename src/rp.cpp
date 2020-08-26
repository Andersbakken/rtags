/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#define RTAGS_SINGLE_THREAD
#include <signal.h>
#include <syslog.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <memory>

#include "ClangIndexer.h"
#include "rct/Log.h"
#include "rct/String.h"
#include "RTags.h"
#include "Server.h"
#include "rct/EventLoop.h"
#include "rct/Flags.h"
#include "rct/Path.h"
#include "rct/Rct.h"

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
    setvbuf(stdout, nullptr, _IONBF, 0);
    setvbuf(stderr, nullptr, _IONBF, 0);
    LogLevel logLevel = LogLevel::Error;
    Path file;
    bool logToSyslog = false;
    bool daemon = false;

    for (int i=1; i<argc; ++i) {
        if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbose")) {
            ++logLevel;
        } else if (!strcmp(argv[i], "--priority")) { // ignore, only for wrapping purposes
            ++i;
        } else if (!strcmp(argv[i], "--log-to-syslog")) {
            logToSyslog = true;
        } else if (!strcmp(argv[i], "--daemon")) {
            daemon = true;
        } else {
            file = argv[i];
        }
    }

    if (const char *env = getenv("TMPDIR")) { // should really always be set by rdm
        Path path = Path(env).ensureTrailingSlash();
        path += String::number(getpid());
        path.mkdir(Path::Recursive);
        setenv("TMPDIR", path.c_str(), 1);
    }
    if (!daemon)
        setenv("LIBCLANG_NOTHREADS", "1", 0);
    signal(SIGSEGV, sigHandler);
    signal(SIGABRT, sigHandler);
    signal(SIGBUS, sigHandler);
    signal(SIGALRM, [](int) {
        ClangIndexer::transition(ClangIndexer::Stopped);
    });

    Flags<LogFlag> logFlags = LogStderr;
    std::shared_ptr<SyslogCloser> closer;
    if (logToSyslog & Server::RPLogToSyslog) {
        logFlags |= LogSyslog;
        closer.reset(new SyslogCloser);
    }
    initLogging(argv[0], logFlags, logLevel);
    (void)closer;

    RTags::initMessages();
    auto eventLoop = std::make_shared<EventLoop>();
    eventLoop->init(EventLoop::MainEventLoop);
    ClangIndexer indexer(daemon ? ClangIndexer::Daemon : ClangIndexer::Normal);
    while (true) {
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
        if (!indexer.exec(data)) {
            error() << "ClangIndexer error";
            return 3;
        }

        if (daemon) {
            if (ClangIndexer::state() == ClangIndexer::Running) {
                printf("@FINISHED@");
                fflush(stdout);
            }
            ClangIndexer::transition(ClangIndexer::NotStarted);
        } else {
            break;
        }
    }

    return 0;
}
