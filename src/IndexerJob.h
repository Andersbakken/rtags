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

#ifndef IndexerJob_h
#define IndexerJob_h

#include "RTags.h"
#include <rct/Hash.h>
#include <rct/ThreadPool.h>
#include <rct/StopWatch.h>
#include <rct/Hash.h>
#include <rct/Process.h>
#include "Source.h"
#include <rct/Flags.h>

class IndexerJob
{
public:
    enum Flag {
        None = 0x000,
        Dirty = 0x001,
        Compile = 0x002,
        Running = 0x010,
        Crashed = 0x020,
        Aborted = 0x040,
        Complete = 0x080,
        Type_Mask = Dirty|Compile
    };

    static String dumpFlags(Flags<Flag> flags);

    IndexerJob(const Source &source,
               Flags<Flag> flags,
               const std::shared_ptr<Project> &project,
               const UnsavedFiles &unsavedFiles = UnsavedFiles());
    void acquireId();
    String encode() const;

    uint64_t id;
    Source source;
    Path sourceFile;
    Flags<Flag> flags;
    Path project;
    int priority;
    enum { HeaderError = -1 };
    UnsavedFiles unsavedFiles;
    Set<uint32_t> visited;
    int crashCount;
private:
    static uint64_t sNextId;
};

RCT_FLAGS(IndexerJob::Flag);

#endif
