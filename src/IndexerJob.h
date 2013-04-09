#ifndef IndexerJob_h
#define IndexerJob_h

#include "RTags.h"
#include "Job.h"
#include <rct/ThreadPool.h>
#include <rct/Mutex.h>

struct IndexData {
    ReferenceMap references;
    SymbolMap symbols;
    SymbolNameMap symbolNames;
    DependencyMap dependencies;
    String message;
    UsrMap usrMap;
    FixItMap fixIts;
    Map<uint32_t, int> errors;
};

class IndexerJob : public Job
{
public:
    enum Type {
        Makefile,
        Dirty,
        Dump
    };
    IndexerJob(const shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation);
    IndexerJob(const QueryMessage &msg, const shared_ptr<Project> &project, const SourceInformation &sourceInformation);
    virtual ~IndexerJob();

    static shared_ptr<IndexerJob> createIndex(const shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation);
    static shared_ptr<IndexerJob> createDump(const QueryMessage &msg, const shared_ptr<Project> &project, const SourceInformation &sourceInformation);

    shared_ptr<IndexData> data() const { return mData; }
    uint32_t fileId() const { return mFileId; }
    Path path() const { return mSourceInformation.sourceFile; }
    bool abortIfStarted();
    const SourceInformation &sourceInformation() const { return mSourceInformation; }
    time_t parseTime() const { return mParseTime; }
    const Set<uint32_t> &visitedFiles() const { return mVisitedFiles; }
    Type type() const { return mType; }
protected:
    virtual void index() = 0;
    virtual void execute();
    
    Location createLocation(uint32_t fileId, uint32_t offset, bool *blocked);
    Location createLocation(const Path &file, uint32_t offset, bool *blocked);
    const Type mType;

    Set<uint32_t> mVisitedFiles, mBlockedFiles;

    Map<String, uint32_t> mFileIds;

    SourceInformation mSourceInformation;
    const uint32_t mFileId;

    StopWatch mTimer;
    shared_ptr<IndexData> mData;

    time_t mParseTime;
    bool mStarted, mAborted;
};

#endif
