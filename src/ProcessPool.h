#ifndef ProcessPool_h
#define ProcessPool_h

#include <rct/Map.h>
#include <rct/Process.h>
#include <rct/ThreadPool.h>
// ### not meant to be threadsafe

class Project;
class ProcessPool
{
public:
    ProcessPool()
        : mCount(ThreadPool::idealThreadCount())
    {}
    ~ProcessPool()
    {
        assert(mActive.isEmpty());
    }
    void setCount(int count) { mCount = count; }
    int count() const { return mCount; }

    void add(const std::shared_ptr<Project> &project, uint32_t fileId);
    void cancel(uint32_t fileId);
    void cancel(const std::shared_ptr<Project> &project);
    void clear();
private:
    void startProcess();
    void onProcessFinished(Process *proc);

    struct Entry {
        uint32_t fileId;
        std::weak_ptr<Project> project;
        enum {
            Pending,
            Active,
            Readded
        } state;
    };
    Map<uint32_t, Entry*> mByFileId;
    Map<Process*, Entry*> mActive;
    List<Entry*> mPending;
    int mCount;
};

#endif
