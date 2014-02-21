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

#define RTAGS_SINGLE_THREAD
#include "ClangIndexer.h"
#include "Cpp.h"
#include "RTagsClang.h"
#include "Source.h"
#include <rct/Log.h>
#include <rct/StopWatch.h>
#include <rct/String.h>
#include <signal.h>
#include <syslog.h>

static bool suspendOnSigSegv = false;
static void sigHandler(int signal)
{
    if (signal == SIGSEGV && suspendOnSigSegv) {
        while (true) {
            fprintf(stderr, "rp crashed..., waiting for debugger\n%d\n", getpid());
            sleep(1);
        }
    }
    error("Caught signal %d\n", signal);
    // this is not really allowed in signal handlers but will mostly work
    const List<String>& trace = RTags::backtrace();
    auto it = trace.cbegin();
    while (it != trace.end()) {
        error("%s", it->constData());
        ++it;
    }
    fflush(stderr);
    ::closelog();
    _exit(1);
}

struct SyslogCloser
{
public:
    ~SyslogCloser() { ::closelog(); }
};

int main(int argc, char **argv)
{
    setenv("LIBCLANG_NOTHREADS", "1", 0);
    signal(SIGSEGV, sigHandler);
    signal(SIGABRT, sigHandler);
    signal(SIGBUS, sigHandler);

    initLogging(argv[0], LogStderr|LogSyslog);
    SyslogCloser closer;
    (void)closer;

    RTags::initMessages();
    std::shared_ptr<EventLoop> eventLoop(new EventLoop);
    eventLoop->init(EventLoop::MainEventLoop);
    String data;
    if (argc > 1) {
        data = Path(argv[1]).readAll();
    } else {
        int size;
        if (!fread(&size, sizeof(size), 1, stdin)) {
            error() << "Failed to read from stdout";
            return 1;
        }
        data.resize(size);
        if (!fread(&data[0], size, 1, stdin)) {
            error() << "Failed to read from stdout";
            return 2;
        }
    }
    Deserializer deserializer(data);
    String destination;
    uint16_t port;
    Path sourceFile;
    Source source;
    Path project;
    uint32_t flags;
    Hash<Path, uint32_t> blockedFiles;
    std::shared_ptr<Cpp> cpp(new Cpp);
    uint64_t jobId;
    uint32_t visitFileTimeout, indexerMessageTimeout, connectTimeout;
    deserializer >> destination >> port >> sourceFile >> source
                 >> *cpp >> project >> flags
                 >> visitFileTimeout >> indexerMessageTimeout >> connectTimeout
                 >> suspendOnSigSegv >> jobId >> blockedFiles;
    if (sourceFile.isEmpty()) {
        error("No sourcefile\n");
        return 3;
    }
    if (!source.fileId) {
        error("Bad fileId\n");
        return 3;
    }

    if (project.isEmpty()) {
        error("No project\n");
        return 4;
    }

    ClangIndexer indexer;
    if (port) {
        if (!indexer.connect(destination, port, connectTimeout)) {
            error("Failed to connect to rdm %s:%d\n", destination.constData(), port);
            return 6;
        }
    } else {
        if (!indexer.connect(destination, connectTimeout)) {
            error("Failed to connect to rdm %s\n", destination.constData());
            return 7;
        }
    }
    Location::init(blockedFiles);
    Location::set(sourceFile, source.fileId);
    indexer.setBlockedFiles(std::move(blockedFiles));
    indexer.setVisitFileTimeout(visitFileTimeout);
    indexer.setIndexerMessageTimeout(indexerMessageTimeout);
    indexer.setJobId(jobId);

    if (!indexer.index(flags, source, cpp, project)) {
        error("Failed to index %s\n", sourceFile.constData());
        return 8;
    }

    return 0;
}
