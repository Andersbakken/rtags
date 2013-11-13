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

#ifndef IndexerJob_h
#define IndexerJob_h

#include "RTags.h"
#include "Job.h"
#include <rct/Hash.h>
#include <rct/ThreadPool.h>
#include <rct/StopWatch.h>
#include <rct/Hash.h>
#include "QueryMessage.h"

class Process;
class IndexerJob : public std::enable_shared_from_this<IndexerJob>
{
public:
    enum IndexType {
        Invalid,
        Makefile,
        Dirty,
        Dump,
        Remote
    };

    IndexerJob(IndexType type, const Path &p, const Source &s, const std::shared_ptr<Cpp> &preprocessed);
    IndexerJob();

    enum State {
        Pending,
        Running,
        Aborted,
        Complete
    };

    bool startLocal();
    bool update(IndexType t, const Source &s, const std::shared_ptr<Cpp> &cpp);
    void abort();
    bool encode(Serializer &serializer);
    void decode(Deserializer &deserializer, Hash<Path, uint32_t> &blockedFiles);
    void onProcessFinished();

    State state;
    String destination;
    uint16_t port;
    IndexType type;
    Path project;
    Source source;
    Path sourceFile;
    Set<uint32_t> visited;
    Process *process;
    Hash<Path, uint32_t> blockedFiles; // only used for remote jobs
    uint64_t id, started;
    std::shared_ptr<Cpp> cpp;

    static uint64_t nextId;
};

class IndexData
{
public:
    IndexData(IndexerJob::IndexType t)
        : aborted(false), parseTime(0), key(0), jobId(0), type(t)
    {}

    Set<uint32_t> visitedFiles() const
    {
        Set<uint32_t> ret;
        for (Hash<uint32_t, bool>::const_iterator it = visited.begin(); it != visited.end(); ++it) {
            if (it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    Set<uint32_t> blockedFiles() const
    {
        Set<uint32_t> ret;
        for (Hash<uint32_t, bool>::const_iterator it = visited.begin(); it != visited.end(); ++it) {
            if (!it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    uint32_t fileId() const
    {
        uint32_t fileId, buildRootId;
        Source::decodeKey(key, fileId, buildRootId);
        return fileId;
    }

    bool aborted;
    uint64_t parseTime, key;
    SymbolMap symbols;
    ReferenceMap references;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    UsrMap usrMap;
    String message; // used as output for dump when type == Dump
    FixItMap fixIts;
    String xmlDiagnostics;
    Hash<uint32_t, bool> visited;
    uint64_t jobId;
    const IndexerJob::IndexType type;
};

#endif
