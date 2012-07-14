#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <ByteArray.h>
#include <List.h>
#include "RTags.h"
#include "Job.h"
#include "Location.h"

class QueryMessage;
class CursorInfoJob : public Job
{
public:
    CursorInfoJob(const Location &loc, const QueryMessage &query);
protected:
    virtual void execute();
private:
    const Location location;
};

#endif
