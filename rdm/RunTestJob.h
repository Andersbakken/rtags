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
    QSet<QByteArray> runJob(Job *job)
    {
        Q_ASSERT(!lines);
        QSet<QByteArray> ret;
        lines = &ret;
        connect(job, SIGNAL(output(int, QByteArray)), this, SLOT(onOutput(int, QByteArray)));
        job->execute();
        lines = 0;
        delete job;
        return ret;
    }
private slots:
    void onOutput(int, const QByteArray &line)
    {
        Q_ASSERT(lines);
        lines->insert(line);
    }
private:
    QSet<QByteArray> *lines;


};

class RunTestJob : public Job
{
    Q_OBJECT
public:
    RunTestJob(const Path &path, int id);
protected:
    virtual void execute();
    void testSymbolNames(const QByteArray &symbolName, const QSet<QByteArray> &expectedLocations);
    QSet<QByteArray> runJob(Job *job) const { JobRunner runner; return runner.runJob(job); }
private:
    const Path path;
};

#endif
