#ifndef IndexerJobRParser_h
#define IndexerJobRParser_h

#include "IndexerJob.h"

class IndexerJobRParser : public IndexerJob
{
public:
    IndexerJobRParser(const std::shared_ptr<Project> &project, Type type,
                      const SourceInformation &sourceInformation);
    IndexerJobRParser(const QueryMessage &msg,
                      const std::shared_ptr<Project> &project,
                      const SourceInformation &sourceInformation);
protected:
    virtual void index();

private:
    friend class Visitor;
};

#endif
