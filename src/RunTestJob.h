#ifndef RunTestJob_h
#define RunTestJob_h

#include "Path.h"
#include "Job.h"

class JobRunner
{
public:
    JobRunner()
    {}
    Set<ByteArray> runJob(Job *job)
    {
        job->setFlags(job->flags() | Job::OutputSignalEnabled);
        job->output().connect(this, &JobRunner::onOutput);
        job->execute();
        delete job;
        Set<ByteArray> ret;
        std::swap(ret, lines);
        return ret;
    }
    void onOutput(const ByteArray &line)
    {
        lines.insert(line);
    }
private:
    Set<ByteArray> lines;
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
