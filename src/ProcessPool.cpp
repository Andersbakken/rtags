#include "ProcessPool.h"

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
    mPending.append(entry);
}

void ProcessPool::cancel(uint32_t fileId)
{
    Entry *entry = mByFileId.take(fileId);
    if (!entry)
        return;

    bool found = false;
    if (entry->state == Entry::Pending) {
        for (List<Entry*>::iterator it = mPending.begin(); it != mPending.end(); ++it) {
            if (it->fileId == fileId) {
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

void ProcessPool::cancelAll(const std::shared_ptr<Project> &project)
{
    assert(project);
    {
        Map<Process*, Entry*>::iterator it = mActive.begin();
        while (it != mActive.end()) {
            if (it->project.lock() == project) {
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
    List<Entry*>::iterator it = mPending.begin();
    while (it != mPending.end()) {
        if (it->project.lock() == project) {
            delete mByFileId.take(it->second->fileId);
            it = mPending.erase(it);
        } else {
            ++it;
        }
    }
}

void ProcessPool::startProcess()
{
    while (mActive.size() < mCount) {
        Process *proc = new Process;
    }
}

void ProcessPool::onProcessFinished(Process *proc)
{
    Entry *entry = mActive.take(proc);
    assert(entry);
    if (entry->state == Entry::Readded) {
        entry->state = Entry::Pending;
        mPending.append(entry);
    } else {
        mByFileId.remove(entry->fileId);
        delete entry;
    }
    startProcess();
}

void ProcessPool::clear()
{

}
