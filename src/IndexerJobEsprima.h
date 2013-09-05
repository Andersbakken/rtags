#ifndef IndexerJobEsprima_h
#define IndexerJobEsprima_h

#include "IndexerJob.h"

class IndexerJobEsprima : public IndexerJob
{
public:
    IndexerJobEsprima(const std::shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation);
    IndexerJobEsprima(const QueryMessage &msg, const std::shared_ptr<Project> &project, const SourceInformation &sourceInformation);
    virtual void index();
};

#endif
