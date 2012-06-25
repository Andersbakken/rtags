#ifndef TestJob_h
#define TestJob_h

#include "Path.h"
#include "Job.h"

class TestJob : public Job
{
public:
    TestJob(const Path &path, int id);
protected:
    virtual void execute();
private:
    const Path path;
};

#endif
