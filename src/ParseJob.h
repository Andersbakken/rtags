#ifndef ParseJob_h
#define ParseJob_h

#include "Job.h"
#include "Path.h"
#include "QueryMessage.h"
#include "AbortInterface.h"

class ParseJob : public Job
{
public:
    ParseJob(const QueryMessage &msg);
    virtual void run();
private:
    const Path mPath;
};

#endif
