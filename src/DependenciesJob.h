#ifndef DependenciesJob_h
#define DependenciesJob_h

#include "Job.h"
#include "Project.h"
#include "QueryMessage.h"

class DependenciesJob : public Job
{
public:
    DependenciesJob(const QueryMessage &query, const shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    uint32_t mFileId;
};

#endif

