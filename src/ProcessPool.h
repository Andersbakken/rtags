#ifndef ProcessPool_h
#define ProcessPool_h

#include <rct/Map.h>
#include <rct/Process.h>
#include "ClangIndexer.h"
// ### not meant to be threadsafe

class Project;
class ProcessPool
{
public:
    ProcessPool();
    ~ProcessPool();
    void setCount(int count) { mCount = count; }
    int count() const { return mCount; }

    void add(const std::shared_ptr<Project> &project, uint32_t fileId, ClangIndexer::Type type);
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
        ClangIndexer::Type type;
        enum {
            Pending,
            Active,
            Readded
        } state;
    };
    Map<uint32_t, Entry*> mByFileId;
    Map<Process*, Entry*> mActive;
    std::deque<Entry*> mPending;
    int mCount;
    const Path mRp;
};

#endif
