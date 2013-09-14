#ifndef ProcessPool_h
#define ProcessPool_h

#include <rct/Map.h>
#include <rct/Process.h>
#include <rct/ThreadPool.h>
// ### not meant to be threadsafe

class ProcessPool
{
public:
    ProcessPool()
        : mCount(ThreadPool::idealThreadCount()), mNextId(1)
    {}
    ~ProcessPool()
    {
        assert(mProcesses.isEmpty());
    }
    void setCount(int count) { mCount = count; }
    int count() const { return mCount; }

    int start(const Path &file, const List<String> &args, const List<String> &environ = List<String>(), const Path &cwd = Path());
    void cancel(int id);
    Signal<std::function<void(int, Process *)> > &finished() { return mFinished; }
    Signal<std::function<void(int, Process *)> > &readyReadStdOut() { return mReadyReadStdOut; }
    Signal<std::function<void(int, Process *)> > &readyReadStdErr() { return mReadyReadStdErr; }
private:
    struct Entry {
        int id;
        Path file;
        List<String> args, environ;
        Path cwd;
    };

    List<Entry> mPending;
    Map<int, Process*> mProcesses;
    Signal<std::function<void(int, Process *)> > mFinished, mReadyReadStdOut, mReadyReadStdErr;
    int mNextId;
};

#endif
