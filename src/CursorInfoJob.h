#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <rct/String.h>
#include <rct/List.h>
#include "RTagsClang.h"
#include "Job.h"
#include "Location.h"

class QueryMessage;
class CursorInfoJob : public Job
{
public:
    CursorInfoJob(const Location &loc, const QueryMessage &query, const shared_ptr<Project> &proj);
protected:
    virtual void execute();
private:
    const Location location;
};

#endif
