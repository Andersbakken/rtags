#ifndef FollowLocationJob_h
#define FollowLocationJob_h

#include "ByteArray.h"
#include "List.h"
#include "RTags.h"
#include "Job.h"
#include "Location.h"

class FollowLocationJob : public Job
{
public:
    FollowLocationJob(const Location &loc, const QueryMessage &query, const std::shared_ptr<Project> &project);
protected:
    virtual void execute();
private:
    const Location location;
};

#endif
