#include <rct/ThreadPool.h>
#include <rct/Rct.h>
#include "Server.h"
#include "Project.h"
#include "ProcessPool.h"

ProcessPool::ProcessPool()
    : mCount(ThreadPool::idealThreadCount())
{
}

ProcessPool::~ProcessPool()
{
}

void ProcessPool::add(const std::shared_ptr<Job> &job)
{
    mPending.push_back(job);
    startProcess();
}

void ProcessPool::startProcess()
{
    while (mActive.size() < mCount && !mPending.empty()) {
        std::shared_ptr<Job> job = mPending.front();
        mPending.pop_front();
        assert(job);
        Process *proc = job->startProcess();
        if (!proc)
            continue;
        assert(!proc->isFinished());
        proc->finished().connect(std::bind(&ProcessPool::onProcessFinished,
                                           this, std::placeholders::_1));
        mActive[proc] = job;
    }
}

void ProcessPool::onProcessFinished(Process *proc)
{
    std::shared_ptr<Job> job = mActive.take(proc);
    assert(job);
    job->finished(proc);
    delete proc;
    startProcess();
}

