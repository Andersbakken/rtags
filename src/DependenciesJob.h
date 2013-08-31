#ifndef DependenciesJob_h
#define DependenciesJob_h

#include "Job.h"
#include "QueryMessage.h"

class Project;
class DependenciesJob : public Job
{
public:
    DependenciesJob(const QueryMessage &query, const std::shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    uint32_t mFileId;
};

#endif

