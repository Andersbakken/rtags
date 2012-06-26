#ifndef RunTestJob_h
#define RunTestJob_h

#include "Path.h"
#include "Job.h"

class JobRunner
{
public:
    JobRunner()
        : lines(0)
    {}
    Set<ByteArray> runJob(Job *job)
    {
        assert(!lines);
        Set<ByteArray> ret;
        lines = &ret;
#warning this is busted
        // connect(job, SIGNAL(output(int, ByteArray)), this, SLOT(onOutput(int, ByteArray)));
        job->execute();
        lines = 0;
        delete job;
        return ret;
    }
    void onOutput(int, const ByteArray &line)
    {
        assert(lines);
        lines->insert(line);
    }
private:
    Set<ByteArray> *lines;


};

class RunTestJob : public Job
{
public:
    RunTestJob(const Path &path, int id);
protected:
    virtual void execute();
    void testSymbolNames(const ByteArray &symbolName, const Set<ByteArray> &expectedLocations);
    Set<ByteArray> runJob(Job *job) const { JobRunner runner; return runner.runJob(job); }
private:
    const Path path;
};

#endif
