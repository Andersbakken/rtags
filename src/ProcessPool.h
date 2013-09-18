#ifndef ProcessPool_h
#define ProcessPool_h

#include <rct/Map.h>
#include <rct/Process.h>
#include "IndexerJob.h"
// ### not meant to be threadsafe

class Project;
class ProcessPool
{
public:
    ProcessPool();
    ~ProcessPool();
    void setCount(int count) { mCount = count; }
    int count() const { return mCount; }

    void add(const std::shared_ptr<Project> &project, uint32_t fileId,
             IndexType type, uint64_t id);
    void cancel(uint32_t fileId);
    void cancel(const std::shared_ptr<Project> &project);
    void clear();
private:
    void startProcess();
    void onProcessFinished(Process *proc);
    void clear(Project *proj);

    struct Entry {
        uint32_t fileId;
        std::weak_ptr<Project> project;
        IndexType type;
        enum {
            Pending,
            Active,
            Readded
        } state;
        uint64_t id;
    };
    Map<uint32_t, Entry*> mByFileId;
    Map<Process*, Entry*> mActive;
    std::deque<Entry*> mPending;
    int mCount;
    const Path mRp;
};

#endif
