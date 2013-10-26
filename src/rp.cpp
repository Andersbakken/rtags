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

#define SINGLE_THREAD
#include "Source.h"
#include "RTagsClang.h"
#include "ClangIndexer.h"
#include <rct/Log.h>
#include <rct/String.h>
#include <rct/StopWatch.h>
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
    String sourceFile;
    Path project;
    List<String> args;
    Path serverFile;
    FILE *f = stdin;
    if (argc > 1) {
        f = fopen(argv[1], "r");
    }
    Deserializer deserializer(f);
    String preprocessed;
    uint32_t fileId;
    uint8_t type;
    int visitFileTimeout, indexerMessageTimeout;
    deserializer >> serverFile >> sourceFile >> fileId >> preprocessed >> args
                 >> project >> type >> visitFileTimeout >> indexerMessageTimeout;
    if (argc > 1)
        fclose(f);
    f = 0;
    if (sourceFile.isEmpty()) {
        fprintf(stderr, "No sourcefile\n");
        return 3;
    }
    if (!fileId) {
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
        break;
    default:
        fprintf(stderr, "Invalid type %d\n", type);
        return 5;
    }

    ClangIndexer indexer;
    if (!indexer.connect(serverFile)) {
        fprintf(stderr, "Failed to connect to rdm\n");
        return 6;
    }
    indexer.setVisitFileTimeout(visitFileTimeout);
    indexer.setIndexerMessageTimeout(indexerMessageTimeout);

    if (!indexer.index(static_cast<IndexerJob::IndexType>(type), project, fileId,
                       sourceFile, preprocessed, args)) {
        fprintf(stderr, "Failed to index %s\n", sourceFile.constData());
        return 7;
    }

    return 0;
}
