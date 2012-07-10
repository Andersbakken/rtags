#ifndef THREADPOOL_H
#define THREADPOOL_H

#include "Mutex.h"
#include "WaitCondition.h"
#include <deque>

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

    protected:
        virtual void run() {}

        void setAutoDelete(bool autoDelete);

    private:
        int mPriority;
        Mutex mMutex;
        bool mAutoDelete;

        friend class ThreadPool;
        friend class ThreadPoolThread;
    };

    enum { Guaranteed = -1 };

    void start(Job* job, int priority = 0);

    static int idealThreadCount();
    static ThreadPool* globalInstance();

private:
    static bool jobLessThan(const Job* l, const Job* r);

private:
    int mConcurrentJobs;
    Mutex mMutex;
    WaitCondition mCond;
    std::deque<Job*> mJobs;
    List<ThreadPoolThread*> mThreads;
    int mBusyThreads;

    static ThreadPool* sGlobalInstance;

    friend class ThreadPoolThread;
};

#endif
