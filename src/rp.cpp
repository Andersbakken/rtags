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

#include "GccArguments.h"
#include "RTagsClang.h"
#include "ClangIndexer.h"
#include <rct/Log.h>
#include <rct/String.h>
#include <rct/StopWatch.h>

int main(int argc, char **argv)
{
    initLogging();
    RTags::initMessages();
    std::shared_ptr<EventLoop> eventLoop(new EventLoop);
    eventLoop->init(EventLoop::MainEventLoop);
    String sourceFile;
    Path project;
    List<String> args;
    Path serverFile;
    uint64_t id;
    Deserializer deserializer(stdin);
    uint32_t fileId;
    uint8_t type;
    deserializer >> serverFile >> sourceFile >> fileId >> project >> args >> type >> id;
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
    case Dirty:
    case Makefile:
    case Dump:
        break;
    default:
        fprintf(stderr, "Invalid type %d\n", type);
        return 5;
    }

    error() << "About to start here" << sourceFile << fileId << args;

    ClangIndexer indexer;
    if (!indexer.connect(serverFile)) {
        fprintf(stderr, "Failed to connect to rdm\n");
        return 6;
    }

    if (!indexer.index(static_cast<IndexType>(type), fileId, project, args, id)) {
        fprintf(stderr, "Failed to index %s\n", sourceFile.constData());
        return 7;
    }

    return 0;
}
