#ifndef ValidateDBJob_h
#define ValidateDBJob_h

#include "Job.h"

class ValidateDBJob : public Job
{
public:
    ValidateDBJob();
protected:
    virtual void execute();
};

#endif
