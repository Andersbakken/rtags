/*
 * (c) 1997-2011 Netflix, Inc.  All content herein is protected by
 * U.S. copyright and other applicable intellectual property laws and
 * may not be copied without the express permission of Netflix, Inc.,
 * which reserves all rights.  Reuse of any of this content for any
 * purpose without the permission of Netflix, Inc. is strictly
 * prohibited.
 */

#ifndef ThreadPool_h
#define ThreadPool_h

#include <QtCore>
class ThreadPoolThread;

class ThreadPoolJob
{
    Q_DISABLE_COPY(ThreadPoolJob);
public:
    ThreadPoolJob() {}
    virtual ~ThreadPoolJob() {}
    virtual void execute() = 0;
    virtual QString description() const { return QString(); }
};

class ThreadPool
{
public:
    ThreadPool();
    ~ThreadPool();
    void init(int threadCount);
    void shutdown();
    void post(ThreadPoolJob *job);
private:
    friend class ::ThreadPoolThread;
    void finished(ThreadPoolThread *thread);
    void exited(ThreadPoolThread *thread);

    QMutex mMutex;
    QWaitCondition mCondition;
    bool mShutdown;
    struct ThreadNode {
        ThreadPoolThread *thread;
        ThreadNode *next;
    } *mWaiting, *mActive, *mExited;
    static ThreadNode *removeThreadNode(ThreadPoolThread *thread, ThreadNode *&list);

    struct JobNode {
        ThreadPoolJob *job;
        JobNode *next;
    } *mBacklogFirst, *mBacklogLast;

    int mBacklogSize;
    enum { MaxBackLogSize = 10000 };
};

#endif
