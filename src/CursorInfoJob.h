#ifndef CursorInfoJob_h
#define CursorInfoJob_h

#include <QObject>
#include <ByteArray.h>
#include <List.h>
#include "RTags.h"
#include "Job.h"
#include "Location.h"

class CursorInfoJob : public Job
{
public:
    CursorInfoJob(int id, const Location &loc, unsigned flags);
    ~CursorInfoJob();
protected:
    virtual void execute();
private:
    const Location location;
    const unsigned flags;
};

#endif
