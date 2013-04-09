#ifndef IndexerJobEsprima_h
#define IndexerJobEsprima_h

#include "IndexerJob.h"

class IndexerJobEsprima : public IndexerJob
{
public:
    IndexerJobEsprima(const shared_ptr<Project> &project, Type type, const SourceInformation &sourceInformation);
    IndexerJobEsprima(const QueryMessage &msg, const shared_ptr<Project> &project, const SourceInformation &sourceInformation);
    virtual void index();
};

#endif
