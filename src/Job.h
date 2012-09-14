#ifndef Job_h
#define Job_h

#include "AbortInterface.h"
#include "ThreadPool.h"
#include <List.h>
#include <ByteArray.h>
#include "Event.h"
#include "SignalSlot.h"
#include "Server.h"
#include "RegExp.h"
#include <tr1/memory>

class CursorInfo;
class Location;
class QueryMessage;
class Project;
class Job : public ThreadPool::Job, public AbortInterface
{
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2,
        OutputSignalEnabled = 0x4,
        WriteBuffered = 0x8
    };
    enum { Priority = 10 };
    Job(const QueryMessage &msg, unsigned jobFlags, const shared_ptr<Project> &proj);
    Job(unsigned jobFlags, const shared_ptr<Project> &project);
    ~Job();

    bool hasFilter() const { return mPathFilters || mPathFiltersRegExp; }
    int id() const { return mId; }
    void setId(int id) { mId = id; }
    enum WriteFlag {
        NoWriteFlags = 0x0,
        IgnoreMax = 0x1
    };
    bool write(const ByteArray &out, unsigned flags = NoWriteFlags);
    bool writeRaw(const ByteArray &out, unsigned flags = NoWriteFlags);
    bool write(const Location &location, const CursorInfo &info, unsigned flags = NoWriteFlags);
    unsigned jobFlags() const { return mJobFlags; }
    void setJobFlags(unsigned flags) { mJobFlags = flags; }
    unsigned queryFlags() const { return mQueryFlags; }
    void setQueryFlags(unsigned queryFlags) { mQueryFlags = queryFlags; }
    unsigned keyFlags() const;
    inline bool filter(const ByteArray &val) const;
    virtual void run();
    virtual void execute() {}
    signalslot::Signal1<const ByteArray &> &output() { return mOutput; }
    shared_ptr<Project> project() const { return mProject; }
private:
    int mId;
    unsigned mJobFlags;
    unsigned mQueryFlags;
    signalslot::Signal1<const ByteArray &> mOutput;
    shared_ptr<Project> mProject;
    List<ByteArray> *mPathFilters;
    List<RegExp> *mPathFiltersRegExp;
    int mMax;
    ByteArray mBuffer;
};

inline bool Job::filter(const ByteArray &val) const
{
    if ((!mPathFilters && !mPathFiltersRegExp)
        || ((!mQueryFlags & QueryMessage::FilterSystemIncludes) && Path::isSystem(val.constData()))) {
        return true;
    } else if (mPathFilters) {
        return RTags::startsWith(*mPathFilters, val);
    }

    const int count = mPathFiltersRegExp->size();
    for (int i=0; i<count; ++i) {
        if (mPathFiltersRegExp->at(i).indexIn(val) != -1)
            return true;

    }
    return false;
}

class JobOutputEvent : public Event
{
public:
    enum { Type = 2 };
    JobOutputEvent(Job *j, const ByteArray &o, bool f)
        : Event(Type), job(j), out(o), finish(f)
    {}

    Job *job;
    const ByteArray out;
    const bool finish;
};

#endif
