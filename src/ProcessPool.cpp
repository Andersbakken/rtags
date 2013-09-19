#include <rct/ThreadPool.h>
#include <rct/Rct.h>
#include "CompilerManager.h"
#include "Server.h"
#include "Project.h"
#include "SourceInformation.h"
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
        String data;
        Path path;
        List<String> args;
        if (!job->init(path, args, data))
            continue;
        Process *proc = new Process;
        if (!proc->start(path, args)) {
            error() << "Couldn't start rp" << proc->errorString();
            job->error(proc->errorString());
            delete proc;
            continue;
        }
        proc->finished().connect(std::bind(&ProcessPool::onProcessFinished,
                                           this, std::placeholders::_1));
        mActive[proc] = job;
        if (!data.isEmpty())
            proc->write(data);
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

