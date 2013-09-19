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
#include <rct/ThreadPool.h>
#include <rct/StopWatch.h>
#include "QueryMessage.h"

enum IndexType {
    Makefile,
    Dirty,
    Dump
};

class IndexData
{
public:
    IndexData(IndexType t, uint64_t id)
        : fileId(0), parseTime(0), aborted(false), id(id), type(t)
    {}
    virtual ~IndexData()
    {}

    Set<uint32_t> visitedFiles() const
    {
        Set<uint32_t> ret;
        for (Map<uint32_t, bool>::const_iterator it = visited.begin(); it != visited.end(); ++it) {
            if (it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    Set<uint32_t> blockedFiles() const
    {
        Set<uint32_t> ret;
        for (Map<uint32_t, bool>::const_iterator it = visited.begin(); it != visited.end(); ++it) {
            if (!it->second)
                ret.insert(it->first);
        }
        return ret;
    }

    uint32_t fileId;
    uint64_t parseTime;
    SymbolMap symbols;
    ReferenceMap references;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    UsrMap usrMap;
    String message;
    String logOutput;
    FixItMap fixIts;
    String xmlDiagnostics;
    Map<uint32_t, bool> visited;
    bool aborted;
    const uint64_t id;
    const IndexType type;
};

class IndexerJob : public std::enable_shared_from_this<IndexerJob>
{
public:
    IndexerJob(uint64_t i, IndexType t, const std::shared_ptr<Project> &p, const SourceInformation &s)
        : id(i), type(t), project(p), sourceInformation(s)
    {}
    IndexerJob(const QueryMessage &q, const std::shared_ptr<Project> &p, const SourceInformation &s)
        : id(0), type(Dump), project(p), sourceInformation(s), queryMessage(q)
    {}

    virtual ~IndexerJob() {}
    virtual void start() = 0;
    virtual bool abort() = 0; // returns true if it was aborted, false if it hadn't started yet
    virtual bool isAborted() const;

    const uint64_t id;
    const IndexType type;
                          std::weak_ptr<Project> project;
    const SourceInformation sourceInformation;
    QueryMessage queryMessage;
};

#endif
