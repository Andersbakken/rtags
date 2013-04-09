#ifndef RTagsPlugin_h
#define RTagsPlugin_h

#include <rct/Memory.h>
#include <rct/Path.h>
#include <rct/List.h>
#include "IndexerJob.h"

class Project;
class QueryMessage;
class SourceInformation;
class RTagsPlugin
{
public:
    virtual ~RTagsPlugin() {}
    virtual shared_ptr<IndexerJob> createJob(const shared_ptr<Project> &project,
                                             IndexerJob::Type type,
                                             const SourceInformation &sourceInformation) = 0;
    virtual shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                             const shared_ptr<Project> &project,
                                             const SourceInformation &sourceInformation) = 0;
    // add functions for completions etc
};

#endif
