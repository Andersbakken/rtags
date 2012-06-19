#ifndef RunTestJob_h
#define RunTestJob_h

#include "Path.h"
#include "Job.h"

class JobRunner : public QObject
{
    Q_OBJECT;
public:
    JobRunner()
        : lines(0)
    {}
    QSet<ByteArray> runJob(Job *job)
    {
        Q_ASSERT(!lines);
        QSet<ByteArray> ret;
        lines = &ret;
        connect(job, SIGNAL(output(int, ByteArray)), this, SLOT(onOutput(int, ByteArray)));
        job->execute();
        lines = 0;
        delete job;
        return ret;
    }
private slots:
    void onOutput(int, const ByteArray &line)
    {
        Q_ASSERT(lines);
        lines->insert(line);
    }
private:
    QSet<ByteArray> *lines;


};

class RunTestJob : public Job
{
    Q_OBJECT
public:
    RunTestJob(const Path &path, int id);
protected:
    virtual void execute();
    void testSymbolNames(const ByteArray &symbolName, const QSet<ByteArray> &expectedLocations);
    QSet<ByteArray> runJob(Job *job) const { JobRunner runner; return runner.runJob(job); }
private:
    const Path path;
};

#endif
