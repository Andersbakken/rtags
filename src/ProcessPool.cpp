#include <rct/ThreadPool.h>
#include <rct/Rct.h>
#include "CompilerManager.h"
#include "Server.h"
#include "Project.h"
#include "SourceInformation.h"
#include "ProcessPool.h"

ProcessPool::ProcessPool()
    : mCount(ThreadPool::idealThreadCount()), mRp(Rct::executablePath().parentDir() + "rp")
{
}

ProcessPool::~ProcessPool()
{
}

void ProcessPool::add(const std::shared_ptr<Project> &project, uint32_t fileId)
{
    Entry *&entry = mByFileId[fileId];
    if (entry) {
        if (entry->state == Entry::Active)
            entry->state = Entry::Readded;
        return;
    }
    entry = new Entry;
    entry->fileId = fileId;
    entry->project = project;
    entry->state = Entry::Pending;
    mPending.push_back(entry);
}

void ProcessPool::cancel(uint32_t fileId)
{
    Entry *entry = mByFileId.take(fileId);
    if (!entry)
        return;

    bool found = false;
    if (entry->state == Entry::Pending) {
        for (std::deque<Entry*>::iterator it = mPending.begin(); it != mPending.end(); ++it) {
            if ((*it)->fileId == fileId) {
                found = true;
                mPending.erase(it);
                break;
            }
        }
    } else {
        for (Map<Process*, Entry*>::iterator it = mActive.begin(); it != mActive.end(); ++it) {
            if (it->second == entry) {
                it->first->stop();
                delete it->first; // ### is this safe
                mActive.erase(it);
                found = true;
                break;
            }
        }
    }
    (void)found;
    assert(found);
    delete entry;
}

void ProcessPool::cancel(const std::shared_ptr<Project> &project)
{
    assert(project);
    {
        Map<Process*, Entry*>::iterator it = mActive.begin();
        while (it != mActive.end()) {
            if (it->second->project.lock() == project) {
                it->first->stop();
                delete it->first; // ### is this safe
                delete mByFileId.take(it->second->fileId);
                mActive.erase(it++);
            } else {
                ++it;
            }
        }
    }
    {
        int i = mPending.size() - 1;
        while (i >= 0) {
            Entry *entry = mPending.at(i);
            if (entry->project.lock() == project) {
                mByFileId.remove(entry->fileId);
                delete entry;
                mPending.erase(mPending.begin() + i);
            }
            --i;
        }
    }
}

void ProcessPool::startProcess()
{
    while (mActive.size() < mCount && !mPending.empty()) {
        Entry *entry = mPending.front();
        std::shared_ptr<Project> project = entry->project.lock();
        if (!project) {
            delete entry;
            continue;
        }

        Process *proc = new Process;
        const SourceInformation sourceInfo = project->sourceInfo(entry->fileId);
        const List<String> args = (sourceInfo.args
                                   + CompilerManager::flags(sourceInfo.compiler)
                                   + Server::instance()->options().defaultArguments
                                   + sourceInfo.sourceFile());
        if (!proc->start(mRp, args)) {
            error() << "Couldn't start rp" << proc->errorString();
            delete proc;
            delete entry;
        } else {
            proc->finished().connect(std::bind(&ProcessPool::onProcessFinished, this));
            mActive[proc] = entry;
        }
    }
}

void ProcessPool::onProcessFinished(Process *proc)
{
    Entry *entry = mActive.take(proc);
    assert(entry);
    if (entry->state == Entry::Readded) {
        entry->state = Entry::Pending;
        mPending.push_back(entry);
    } else {
        mByFileId.remove(entry->fileId);
        delete entry;
    }
    startProcess();
}

void ProcessPool::clear()
{

}
