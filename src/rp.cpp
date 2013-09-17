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
    std::shared_ptr<EventLoop> eventLoop(new EventLoop);
    eventLoop->init(EventLoop::MainEventLoop);
    String sourceFile;
    Path project;
    List<String> args;
    ClangIndexer::Type type = ClangIndexer::Dirty;
    Path serverFile;

    if (argc == 1) {
        String out;
        Rct::readFile(stdin, out);
        Deserializer deserializer(out);
        uint8_t t;
        deserializer >> serverFile >> sourceFile >> project >> args >> t;
        type = static_cast<ClangIndexer::Type>(t);
    } else {
        serverFile = String::format<128>("%s.rdm", Path::home().constData());
        for (int i=1; i<argc - 1; ++i)
            args[i - 1] = argv[i];
        GccArguments a;
        if (!a.parse(args, Path::pwd())) {
            fprintf(stderr, "Failed to parse command line\n");
            return 2;
        }
        project = a.projectRoot();
        sourceFile = a.inputFiles().value(0);
        args = a.clangArgs();
    }
    if (sourceFile.isEmpty()) {
        fprintf(stderr, "No sourcefile\n");
        return 3;
    }

    ClangIndexer indexer;
    if (!indexer.connect(serverFile)) {
        fprintf(stderr, "Failed to connect to rdm\n");
        return 4;
    }
    if (!indexer.index(type, sourceFile, project, args)) {
        fprintf(stderr, "Failed to index %s\n", sourceFile.constData());
        return 5;
    }

    return 0;
}
