#ifndef JSONJob_h
#define JSONJob_h

#include <rct/String.h>
#include "Job.h"
#include "RTags.h"
#include "Match.h"

class QueryMessage;
class JSONJob : public Job
{
public:
    JSONJob(const QueryMessage &query, const std::shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    const Match match;
};

#endif
