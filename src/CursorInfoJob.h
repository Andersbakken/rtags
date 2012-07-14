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
    CursorInfoJob(int id, const Location &loc, const QueryMessage &query);
    ~CursorInfoJob();
protected:
    virtual void execute();
private:
    const Location location;
};

#endif
