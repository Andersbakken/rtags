#ifndef RTagsPlugin_h
#define RTagsPlugin_h

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
    virtual std::shared_ptr<IndexerJob> createJob(const std::shared_ptr<Project> &project,
                                             IndexerJob::Type type,
                                             const SourceInformation &sourceInformation) = 0;
    virtual std::shared_ptr<IndexerJob> createJob(const QueryMessage &msg,
                                             const std::shared_ptr<Project> &project,
                                             const SourceInformation &sourceInformation) = 0;
    // add functions for completions etc
};

#endif
