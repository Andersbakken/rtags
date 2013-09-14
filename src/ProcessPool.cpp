#include "ProcessPool.h"

int ProcessPool::start(const Path &file, const List<String> &args, const List<String> &environ, const Path &cwd)
{
    if (mProcesses.size() >= mCount) {
        mPending.push_back(Entry());
        Entry &pending = mPending.last();
        pending.id = mNextId++;
        pending.file = file;
        pending.args = args;
        pending.environ = environ;
        pending.cwd = cwd;
        return pending.id;
    }

}

void ProcessPool::cancel(int id)
{

}
