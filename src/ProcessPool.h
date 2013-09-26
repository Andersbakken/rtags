#ifndef ProcessPool_h
#define ProcessPool_h

#include <list>
#include <rct/Hash.h>
#include <rct/Process.h>
// ### not meant to be threadsafe

class ProcessPool
{
public:
    ProcessPool();
    ~ProcessPool();
    void setCount(int count) { mCount = count; }
    int count() const { return mCount; }

    class Job
    {
    public:
        virtual ~Job() {}
        virtual Process *startProcess() = 0;
        virtual void finished(Process *process) = 0;
    };

    void add(const std::shared_ptr<Job> &job);
private:
    void startProcess();
    void onProcessFinished(Process *proc);
    void clear(Project *proj);

    Hash<Process*, std::shared_ptr<Job> > mActive;
    std::list<std::shared_ptr<Job> > mPending;
    int mCount;
};

#endif
