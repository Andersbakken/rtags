#ifndef RunTestJob_h
#define RunTestJob_h

#include "Path.h"
#include "Job.h"

class RunTestJob : public Job
{
    Q_OBJECT
public:
    RunTestJob(const Path &path, int id);
protected:
    virtual void execute();
    QList<QByteArray> runJob(Job *job);
public slots:
    void onOutput(int, const QByteArray &out) { messages.append(out); }
private:
    QList<QByteArray> messages;
    const Path path;
};

#endif
