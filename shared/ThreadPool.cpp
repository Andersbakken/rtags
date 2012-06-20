#include "ThreadPool.h"
#include "Thread.h"
#include "MutexLocker.h"
#include <algorithm>
#include <assert.h>
#if defined (FreeBSD) || defined (NetBSD) || defined (OpenBSD) || defined (bsdi)
#   include <sys/types.h>
#   include <sys/sysctl.h>
#elif defined (linux)
#   include <unistd.h>
#endif

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
        if (mPool->mJobs.empty()) {
            mPool->mCond.wait(&mPool->mMutex);
        }
        else {
            std::vector<ThreadPool::Job*>::iterator item = mPool->mJobs.begin();
            assert(item != mPool->mJobs.end());
            ThreadPool::Job* job = *item;
            mPool->mJobs.erase(item);
            MutexLocker jobLocker(&job->mMutex);
            locker.unlock();
            job->run();
            if (job->mAutoDelete) {
                jobLocker.unlock();
                delete job;
            }
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
    for (std::vector<Job*>::const_iterator it = mJobs.begin();
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

void ThreadPool::setConcurrentJobs(int concurrentJobs)
{
    if (concurrentJobs == mConcurrentJobs)
        return;
    if (concurrentJobs > mConcurrentJobs) {
        MutexLocker locker(&mMutex);
        for (int i = mConcurrentJobs; i < concurrentJobs; ++i) {
            mThreads.push_back(new ThreadPoolThread(this));
            mThreads.back()->start();
        }
        mConcurrentJobs = concurrentJobs;
    } else {
        MutexLocker locker(&mMutex);
        for (int i = mConcurrentJobs; i > concurrentJobs; --i) {
            ThreadPoolThread* t = mThreads.back();
            mThreads.pop_back();
            locker.unlock();
            t->stop();
            t->join();
            locker.relock();
            delete t;
        }
    }
}

bool ThreadPool::jobLessThan(const Job* l, const Job* r)
{
    return l->mPriority < r->mPriority;
}

void ThreadPool::start(Job* job, int priority)
{
    job->mPriority = priority;
    MutexLocker locker(&mMutex);
    mJobs.push_back(job);
    std::sort(mJobs.begin(), mJobs.end(), jobLessThan);
    mCond.wakeOne();
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
#   warning idealthreadcount not implemented on this platform
    return 1;
#endif
}

ThreadPool* ThreadPool::globalInstance()
{
    if (!sGlobalInstance)
        sGlobalInstance = new ThreadPool(idealThreadCount());
    return sGlobalInstance;
}

ThreadPool::Job::Job()
    : mPriority(0), mAutoDelete(true)
{
}

ThreadPool::Job::~Job()
{
    // hold the mutex when deleting in order to ensure that run() is done
    MutexLocker jobLocker(&mMutex);
}

void ThreadPool::Job::setAutoDelete(bool autoDelete)
{
    MutexLocker locker(&mMutex);
    mAutoDelete = autoDelete;
}
