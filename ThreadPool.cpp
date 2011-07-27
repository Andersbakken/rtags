#include "ThreadPool.h"

class ThreadPoolThread : public QThread
{
public:
    ThreadPoolThread(ThreadPool *pool)
        : QThread(), mPool(pool), mExiting(false), mJob(0), mMutex(QMutex::NonRecursive), exitCount(0)
    {
    }

    ~ThreadPoolThread()
    {
        delete mJob;
        wait();
    }

    void startJob(ThreadPoolJob *job)
    {
        QMutexLocker lock(&mMutex);
        Q_ASSERT(!mJob);
        Q_ASSERT(!mExiting);
        mJob = job;
        mCondition.wakeOne();
    }

    void exit()
    {
        QMutexLocker lock(&mMutex);
        mExiting = true;
        mCondition.wakeOne();
    }
protected:
    void run()
    {
        while (true) {
            ThreadPoolJob *job = 0;
            {
                QMutexLocker lock(&mMutex);
                while (!mJob && !mExiting)
                    mCondition.wait(&mMutex);
                if (mExiting)
                    break;
                Q_ASSERT(mJob);
                job = mJob;
                mJob = 0;
            }
            Q_ASSERT(job);
            job->execute();
            delete job;
            mPool->finished(this);
        }
        mPool->exited(this);
    }
private:
    ThreadPool *mPool;
    bool mExiting;
    ThreadPoolJob *mJob;
    QMutex mMutex;
    QWaitCondition mCondition;
    int exitCount;
};

ThreadPool::ThreadPool()
    : mMutex(QMutex::NonRecursive), mShutdown(false), mWaiting(0), mActive(0),
      mExited(0), mBacklogFirst(0), mBacklogLast(0), mBacklogSize(0)
{
}

ThreadPool::~ThreadPool()
{
    shutdown();
}


void ThreadPool::init(int threadCount)
{
    mShutdown = false;
    Q_ASSERT(!mWaiting && !mActive && !mExited);
    for (int i=0; i<threadCount; ++i) {
        ThreadNode *node = new ThreadNode;
        node->next = mWaiting;
        node->thread = new ThreadPoolThread(this);
        node->thread->start();
        mWaiting = node;
    }
}

void ThreadPool::shutdown()
{
    QMutexLocker lock(&mMutex);
    mShutdown = true; // no more jobs should arrive

    while (mActive) {
        mActive->thread->exit();
        mCondition.wait(&mMutex);
    }

    while (mWaiting) {
        mWaiting->thread->exit();
        mCondition.wait(&mMutex);
    }

    Q_ASSERT(!mActive && !mWaiting);
    while (mExited) {
        ThreadNode *node = mExited;
        mExited = node->next;
        delete node->thread;
        delete node;
    }

    int count = 0;
    while (mBacklogFirst) {
        JobNode *node = mBacklogFirst;
        mBacklogFirst = mBacklogFirst->next;
        delete node;
        ++count;
    }
    mBacklogSize = 0;
    mBacklogFirst = mBacklogLast = 0;
}

void ThreadPool::post(ThreadPoolJob *job)
{
    QMutexLocker lock(&mMutex);
    Q_ASSERT(mWaiting || mActive || mExited);
    Q_ASSERT(job);
    if (mShutdown) {
        delete job;
        return;
    }
    if (mWaiting) {
        ThreadNode *node = mWaiting;
        mWaiting = mWaiting->next;
        node->next = mActive;
        mActive = node;
        node->thread->startJob(job);
    } else if (mBacklogSize >= MaxBackLogSize) {
        delete job;
    } else {
        ++mBacklogSize;
        JobNode *node = new JobNode;
        node->next = 0;
        node->job = job;
        if (!mBacklogFirst) {
            mBacklogFirst = mBacklogLast = node;
        } else {
            mBacklogLast->next = node;
            mBacklogLast = node;
        }
    }
}

void ThreadPool::exited(ThreadPoolThread *thread)
{
    Q_ASSERT(thread);
    QMutexLocker lock(&mMutex);
    Q_ASSERT(mWaiting);
    ThreadNode *node = removeThreadNode(thread, mWaiting);
    Q_ASSERT(node);
    node->next = mExited;
    mExited = node;
    mCondition.wakeOne();
}

void ThreadPool::finished(ThreadPoolThread *thread)
{
    Q_ASSERT(thread);
    QMutexLocker lock(&mMutex);
    Q_ASSERT(mActive);

    if (mBacklogFirst && !mShutdown) {
        JobNode *node = mBacklogFirst;
        --mBacklogSize;
        mBacklogFirst = mBacklogFirst->next;
        if (!mBacklogFirst)
            mBacklogLast = 0;
        thread->startJob(node->job);
        delete node;
    } else {
        ThreadNode *node = removeThreadNode(thread, mActive);
        Q_ASSERT(node);
        node->next = mWaiting;
        mWaiting = node;
    }
    mCondition.wakeOne();
}

// This function is always called with lock held
ThreadPool::ThreadNode *ThreadPool::removeThreadNode(ThreadPoolThread *thread, ThreadNode *&list)
{
    Q_ASSERT(thread);
    Q_ASSERT(list);
    ThreadNode *prev = 0;
    ThreadNode *node = list;
    while (node) {
        if (node->thread == thread) {
            if (prev) {
                prev->next = node->next;
            } else {
                list = node->next;
            }
            node->next = 0;
            break;
        }
        prev = node;
        node = node->next;
    }
    Q_ASSERT(node);
    return node;
}


