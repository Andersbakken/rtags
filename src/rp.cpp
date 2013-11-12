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

#include "ClangIndexer.h"
#include "Cpp.h"
#include "RTagsClang.h"
#include "Source.h"
#include <rct/Log.h>
#include <rct/StopWatch.h>
#include <rct/String.h>
#include <signal.h>

static void sigSegvHandler(int signal)
{
    fprintf(stderr, "Caught signal %d\n", signal);
    const String trace = RTags::backtrace();
    if (!trace.isEmpty()) {
        fprintf(stderr, "%s", trace.constData());
    }
    fflush(stderr);
    _exit(1);
}

int main(int argc, char **argv)
{
    signal(SIGSEGV, sigSegvHandler);
    initLogging();
    RTags::initMessages();
    std::shared_ptr<EventLoop> eventLoop(new EventLoop);
    eventLoop->init(EventLoop::MainEventLoop);
    FILE *f = stdin;
    if (argc > 1) {
        f = fopen(argv[1], "r");
    }
    Deserializer deserializer(f);
    String destination;
    uint16_t port;
    Path sourceFile;
    Source source;
    Path project;
    uint8_t type;
    Hash<Path, uint32_t> blockedFiles;
    std::shared_ptr<Cpp> cpp(new Cpp);
    uint64_t jobId;
    int visitFileTimeout, indexerMessageTimeout;
    deserializer >> destination >> port >> sourceFile >> source
                 >> *cpp >> project >> type
                 >> visitFileTimeout >> indexerMessageTimeout
                 >> jobId >> blockedFiles;
    if (argc > 1)
        fclose(f);
    f = 0;
    if (sourceFile.isEmpty()) {
        fprintf(stderr, "No sourcefile\n");
        return 3;
    }
    if (!source.fileId) {
        fprintf(stderr, "Bad fileId\n");
        return 3;
    }

    if (project.isEmpty()) {
        fprintf(stderr, "No project\n");
        return 4;
    }

    switch (type) {
    case IndexerJob::Dirty:
    case IndexerJob::Makefile:
    case IndexerJob::Dump:
    case IndexerJob::Remote:
        break;
    default:
        fprintf(stderr, "Invalid type %d\n", type);
        return 5;
    }

    ClangIndexer indexer;
    if (port) {
        if (!indexer.connect(destination, port)) {
            fprintf(stderr, "Failed to connect to rdm %s:%d\n", destination.constData(), port);
            return 6;
        }
    } else {
        if (!indexer.connect(destination)) {
            fprintf(stderr, "Failed to connect to rdm %s\n", destination.constData());
            return 7;
        }
    }
    Location::init(blockedFiles);
    Location::set(sourceFile, source.fileId);
    indexer.setBlockedFiles(std::move(blockedFiles));
    indexer.setVisitFileTimeout(visitFileTimeout);
    indexer.setIndexerMessageTimeout(indexerMessageTimeout);
    indexer.setJobId(jobId);

    if (!indexer.index(static_cast<IndexerJob::IndexType>(type), source, cpp, project)) {
        fprintf(stderr, "Failed to index %s\n", sourceFile.constData());
        return 8;
    }

    return 0;
}
