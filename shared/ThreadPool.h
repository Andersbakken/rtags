#ifndef THREADPOOL_H
#define THREADPOOL_H

#include "Mutex.h"
#include "WaitCondition.h"
#include <set>
#include <vector>

class ThreadPoolThread;

class ThreadPool
{
public:
    ThreadPool(int concurrentJobs);
    ~ThreadPool();

    class Job
    {
    public:
        Job();
        virtual ~Job();

    protected:
        virtual void run() = 0;

    private:
        int mPriority;

        friend class ThreadPool;
        friend class ThreadPoolThread;
    };

    void start(Job* job, int priority = 0);

    static int idealThreadCount();
    static ThreadPool* globalInstance();

private:
    struct JobLessThan : public std::binary_function<Job, Job*, bool>
    {
        bool operator()(const Job* l, const Job* r) const
        {
            return l->mPriority < r->mPriority;
        }
    };

private:
    int mConcurrentJobs;
    Mutex mMutex;
    WaitCondition mCond;
    std::set<Job*, JobLessThan> mJobs;
    std::vector<ThreadPoolThread*> mThreads;

    static ThreadPool* sGlobalInstance;

    friend class ThreadPoolThread;
};

#endif
