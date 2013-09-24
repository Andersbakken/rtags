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

class IndexData
{
public:
    enum {
        ClangType = 1
    };
    IndexData(int t = 0)
        : type(t)
    {}
    virtual ~IndexData()
    {}

    ReferenceMap references;
    SymbolMap symbols;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    String message;
    UsrMap usrMap;
    FixItMap fixIts;
    Map<uint32_t, int> errors;
    const int type;
};

class IndexerJob : public Job
{
public:
    typedef std::shared_ptr<IndexerJob> SharedPtr;
    enum Type {
        Makefile,
        Dirty,
        Dump
    };
    IndexerJob(const std::shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation);
    IndexerJob(const QueryMessage &msg, const std::shared_ptr<Project> &project, const SourceInformation &sourceInformation);
    virtual ~IndexerJob();
    std::shared_ptr<IndexData> data() const { return mData; }
    uint32_t fileId() const { return mSourceInformation.fileId; }
    Path path() const { return mSourceInformation.sourceFile(); }
    bool abortIfStarted();
    const SourceInformation &sourceInformation() const { return mSourceInformation; }
    time_t parseTime() const { return mParseTime; }
    const Set<uint32_t> &visitedFiles() const { return mVisitedFiles; }
    const Set<uint32_t> &blockedFiles() const { return mBlockedFiles; }
    Type type() const { return mType; }
    Signal<std::function<void(IndexerJob::SharedPtr)> >& finished() { return mFinished; }
protected:
    virtual void index() = 0;
    virtual void execute();
    virtual std::shared_ptr<IndexData> createIndexData() { return std::shared_ptr<IndexData>(new IndexData); }

    Location createLocation(uint32_t fileId, uint32_t offset, bool *blocked);
    Location createLocation(const Path &file, uint32_t offset, bool *blocked);
    const Type mType;

    // FILE *mLogFile;

    Set<uint32_t> mVisitedFiles, mBlockedFiles;

    Map<String, uint32_t> mFileIds;

    SourceInformation mSourceInformation;

    StopWatch mTimer;
    std::shared_ptr<IndexData> mData;

    time_t mParseTime;
    bool mStarted;

    Signal<std::function<void(IndexerJob::SharedPtr)> > mFinished;
};

#endif
