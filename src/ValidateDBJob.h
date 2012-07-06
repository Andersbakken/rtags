#ifndef ValidateDBJob_h
#define ValidateDBJob_h

#include "Job.h"

class ValidateDBJob : public Job
{
public:
    ValidateDBJob(const Path &root);
protected:
    virtual void execute();
private:
    const Path mRoot;
};

#endif
