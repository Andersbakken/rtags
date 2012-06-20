#include "ThreadPool.h"
#include "Thread.h"
#include "MutexLocker.h"
#include <assert.h>
#include <unistd.h>

ThreadPool* ThreadPool::sGlobalInstance = 0;

class ThreadPoolThread : public Thread
{
public:
    ThreadPoolThread(ThreadPool* pool);
    ~ThreadPoolThread();

    void stop();

protected:
    virtual void run();

private:
    ThreadPool* mPool;
    bool mStopped;
};

ThreadPoolThread::ThreadPoolThread(ThreadPool* pool)
    : mPool(pool), mStopped(false)
{
}

ThreadPoolThread::~ThreadPoolThread()
{
}

void ThreadPoolThread::stop()
{
    MutexLocker locker(&mPool->mMutex);
    mStopped = true;
    mPool->mCond.wakeAll();
}

void ThreadPoolThread::run()
{
    for (;;) {
        MutexLocker locker(&mPool->mMutex);
        if (mStopped)
            break;
        if (mPool->mJobs.empty())
            mPool->mCond.wait(&mPool->mMutex);
        else {
            std::set<ThreadPool::Job*>::iterator item = mPool->mJobs.begin();
            assert(item != mPool->mJobs.end());
            ThreadPool::Job* job = *item;
            mPool->mJobs.erase(item);
            locker.unlock();
            job->run();
            delete job;
        }
    }
}

ThreadPool::ThreadPool(int concurrentJobs)
    : mConcurrentJobs(concurrentJobs)
{
    for (int i = 0; i < mConcurrentJobs; ++i) {
        mThreads.push_back(new ThreadPoolThread(this));
        mThreads.back()->start();
    }
}

ThreadPool::~ThreadPool()
{
    MutexLocker locker(&mMutex);
    for (std::set<Job*>::const_iterator it = mJobs.begin();
         it != mJobs.end(); ++it) {
        delete *it;
    }
    mJobs.clear();
    locker.unlock();
    for (std::vector<ThreadPoolThread*>::iterator it = mThreads.begin();
         it != mThreads.end(); ++it) {
        ThreadPoolThread* t = *it;
        t->stop();
        t->join();
        delete t;
    }
}

void ThreadPool::start(Job* job, int priority)
{
    job->mPriority = priority;
    MutexLocker locker(&mMutex);
    mJobs.insert(job);
}

int ThreadPool::idealThreadCount()
{
#if defined (FreeBSD) || defined (NetBSD) || defined (OpenBSD) || defined (bsdi)
    int cores;
    size_t len = sizeof(cores);
    int mib[2];
    mib[0] = CTL_HW;
    mib[1] = HW_NCPU;
    if (sysctl(mib, 2, &cores, &len, NULL, 0) != 0)
        return 1;
    return cores;
#elif defined (linux)
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
#else
#warning idealthreadcount not implemented on this platform
    return 1;
#endif
}

ThreadPool* ThreadPool::globalInstance()
{
    if (!sGlobalInstance)
        sGlobalInstance = new ThreadPool(idealThreadCount());
    return sGlobalInstance;
}
