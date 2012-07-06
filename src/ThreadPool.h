#ifndef THREADPOOL_H
#define THREADPOOL_H

#include "Mutex.h"
#include "WaitCondition.h"
#include <vector>
#include <signalslot.h>

class ThreadPoolThread;

class ThreadPool
{
public:
    ThreadPool(int concurrentJobs);
    ~ThreadPool();

    void setConcurrentJobs(int concurrentJobs);

    class Job
    {
    public:
        Job();
        virtual ~Job();
        signalslot::Signal1<ThreadPool::Job*> &finished() { return mFinished; }

    protected:
        virtual void run() = 0;

        void setAutoDelete(bool autoDelete);

    private:
        int mPriority;
        Mutex mMutex;
        bool mAutoDelete;
        signalslot::Signal1<ThreadPool::Job*> mFinished;

        friend class ThreadPool;
        friend class ThreadPoolThread;
    };

    void start(Job* job, int priority = 0);

    static int idealThreadCount();
    static ThreadPool* globalInstance();

private:
    static bool jobLessThan(const Job* l, const Job* r);

private:
    int mConcurrentJobs;
    Mutex mMutex;
    WaitCondition mCond;
    std::vector<Job*> mJobs;
    std::vector<ThreadPoolThread*> mThreads;

    static ThreadPool* sGlobalInstance;

    friend class ThreadPoolThread;
};

#endif
