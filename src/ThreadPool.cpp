#include "ThreadPool.h"
#include "Thread.h"
#include "MutexLocker.h"
#include <algorithm>
#include <assert.h>
#if defined (OS_FreeBSD) || defined (OS_NetBSD) || defined (OS_OpenBSD)
#   include <sys/types.h>
#   include <sys/sysctl.h>
#elif defined (OS_Linux)
#   include <unistd.h>
#elif defined (OS_Darwin)
#   include <sys/param.h>
#   include <sys/sysctl.h>
#endif

ThreadPool* ThreadPool::sGlobalInstance = 0;

class ThreadPoolThread : public Thread
{
public:
    ThreadPoolThread(ThreadPool* pool);
    ThreadPoolThread(ThreadPool::Job* job);

    void stop();

protected:
    virtual void run();

private:
    ThreadPool::Job *mJob;
    ThreadPool* mPool;
    bool mStopped;
};

ThreadPoolThread::ThreadPoolThread(ThreadPool* pool)
    : mJob(0), mPool(pool), mStopped(false)
{
    setAutoDelete(false);
}

ThreadPoolThread::ThreadPoolThread(ThreadPool::Job* job)
    : mJob(job), mPool(0), mStopped(false)
{
    setAutoDelete(false);
}

void ThreadPoolThread::stop()
{
    MutexLocker locker(&mPool->mMutex);
    mStopped = true;
    mPool->mCond.wakeAll();
}

void ThreadPoolThread::run()
{
    if (mJob) {
        mJob->mMutex.lock();
        mJob->run();
        const bool isAutoDelete = mJob->mAutoDelete;
        mJob->mMutex.unlock();
        if (isAutoDelete) {
            delete mJob;
        }
        return;
    }
    bool first = true;
    for (;;) {
        MutexLocker locker(&mPool->mMutex);
        if (!first) {
            --mPool->mBusyThreads;
        } else {
            first = false;
        }
        while (mPool->mJobs.empty() && !mStopped)
            mPool->mCond.wait(&mPool->mMutex);
        if (mStopped)
            break;
        std::deque<ThreadPool::Job*>::iterator item = mPool->mJobs.begin();
        assert(item != mPool->mJobs.end());
        ThreadPool::Job* job = *item;
        mPool->mJobs.erase(item);
        job->mMutex.lock();
        ++mPool->mBusyThreads;
        locker.unlock();
        job->run();
        const bool isAutoDelete = job->mAutoDelete;
        job->mMutex.unlock();
        if (isAutoDelete) {
            delete job;
        }
    }
}

ThreadPool::ThreadPool(int concurrentJobs)
    : mConcurrentJobs(concurrentJobs), mBusyThreads(0)
{
    for (int i = 0; i < mConcurrentJobs; ++i) {
        mThreads.push_back(new ThreadPoolThread(this));
        mThreads.back()->start();
    }
}

ThreadPool::~ThreadPool()
{
    MutexLocker locker(&mMutex);
    for (std::deque<Job*>::const_iterator it = mJobs.begin();
         it != mJobs.end(); ++it) {
        delete *it;
    }
    mJobs.clear();
    locker.unlock();
    for (List<ThreadPoolThread*>::iterator it = mThreads.begin();
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
        mConcurrentJobs = concurrentJobs;
    }
}

bool ThreadPool::jobLessThan(const Job* l, const Job* r)
{
    return static_cast<unsigned>(l->mPriority) > static_cast<unsigned>(r->mPriority);
}

void ThreadPool::start(Job* job, int priority)
{
    job->mPriority = priority;
    if (priority == Guaranteed) {
        ThreadPoolThread *t = new ThreadPoolThread(job);
        t->start();
        return;
    }

    MutexLocker locker(&mMutex);
    if (mJobs.empty()) {
        mJobs.push_back(job);
    } else {
        if (mJobs.at(mJobs.size() - 1)->mPriority >= priority) {
            mJobs.push_back(job);
        } else if (mJobs.at(0)->mPriority < priority) {
            mJobs.push_front(job);
        } else {
            mJobs.push_back(job);
            std::sort(mJobs.begin(), mJobs.end(), jobLessThan);
        }
    }
    mCond.wakeOne();
}

int ThreadPool::idealThreadCount()
{
#if defined (OS_FreeBSD) || defined (OS_NetBSD) || defined (OS_OpenBSD)
    int cores;
    size_t len = sizeof(cores);
    int mib[2];
    mib[0] = CTL_HW;
    mib[1] = HW_NCPU;
    if (sysctl(mib, 2, &cores, &len, NULL, 0) != 0)
        return 1;
    return cores;
#elif defined (OS_Linux)
    return (int)sysconf(_SC_NPROCESSORS_ONLN);
#elif defined (OS_Darwin)
    int cores;
    size_t len = sizeof(cores);
    int mib[2] = { CTL_HW, HW_AVAILCPU };
    if (sysctl(mib, 2, &cores, &len, NULL, 0)) {
        mib[1] = HW_NCPU;
        if (sysctl(mib, 2, &cores, &len, NULL, 0))
            return 1;
    }
    return cores;
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
