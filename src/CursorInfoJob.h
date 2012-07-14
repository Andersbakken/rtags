#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <ByteArray.h>
#include <List.h>
#include "RTags.h"
#include "Job.h"
#include "Location.h"

class CursorInfoJob : public Job
{
public:
    CursorInfoJob(int id, const Location &loc, unsigned queryFlags);
    ~CursorInfoJob();
protected:
    virtual void execute();
private:
    const Location location;
};

#endif
