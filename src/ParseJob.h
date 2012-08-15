#ifndef ParseJob_h
#define ParseJob_h

#include "Job.h"
#include "Path.h"
#include "QueryMessage.h"
#include "AbortInterface.h"

class ParseJob : public Job
{
public:
    ParseJob(const QueryMessage &msg, const shared_ptr<Project> &proj);
    virtual void run();
private:
    const Path mPath;
};

#endif
