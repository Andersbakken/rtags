#ifndef GRParseJob_h
#define GRParseJob_h

#include "RTags.h"
#include "ThreadPool.h"
#include "Path.h"
#include "AbortInterface.h"
#include "SignalSlot.h"
#include "Location.h"

class Project;
class GRParseJob : public ThreadPool::Job, public AbortInterface, public std::enable_shared_from_this<GRParseJob>
{
public:
    enum Flag {
        None = 0x0,
        Dirty = 0x1
    };
    GRParseJob(const Path &path, unsigned flags, const std::shared_ptr<Project> &project);
    const Path &path() const { return mPath; }
    unsigned flags() const { return mFlags; }
    virtual void run();
    time_t parseTime() const { return mParseTime; }
    signalslot::Signal2<const std::shared_ptr<GRParseJob> &, const GRMap &> &finished() { return mFinished; }
private:
    signalslot::Signal2<const std::shared_ptr<GRParseJob> &, const GRMap &> mFinished;

    GRMap mEntries;
    const Path mPath;
    const unsigned mFlags;
    time_t mParseTime;
    std::weak_ptr<Project> mProject;
};

#endif
