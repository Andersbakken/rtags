#ifndef Job_h
#define Job_h

#include "AbortInterface.h"
#include "ThreadPool.h"
#include <List.h>
#include <ByteArray.h>
#include "Event.h"
#include "signalslot.h"

class CursorInfo;
class Location;
class Job : public ThreadPool::Job, public AbortInterface
{
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2,
        OutputSignalEnabled = 0x4
    };
    Job(unsigned jobFlags, unsigned queryFlags);
    ~Job();
    void setPathFilters(const List<ByteArray> &filter);
    List<ByteArray> pathFilters() const;
    int id() const { return mId; }
    void setId(int id) { mId = id; }
    void write(const ByteArray &out);
    void writeRaw(const ByteArray &out);
    unsigned jobFlags() const { return mJobFlags; }
    void setJobFlags(unsigned flags) { mJobFlags = flags; }
    unsigned queryFlags() const { return mQueryFlags; }
    void setQueryFlags(unsigned queryFlags) { mQueryFlags = queryFlags; }
    unsigned keyFlags() const;
    bool filter(const ByteArray &val) const;
    virtual void run();
    virtual void execute() {}
    signalslot::Signal1<const ByteArray &> &output() { return mOutput; }
    void write(const Location &location, const CursorInfo &info);
private:
    int mId;
    unsigned mJobFlags;
    unsigned mQueryFlags;
    List<ByteArray> mPathFilters;
    signalslot::Signal1<const ByteArray &> mOutput;
};

class JobCompleteEvent : public Event
{
public:
    enum { Type = 1 };
    JobCompleteEvent(Job *j)
        : Event(Type), job(j)
    {}

    Job *job;
};


class JobOutputEvent : public Event
{
public:
    enum { Type = 2 };
    JobOutputEvent(Job *j, const ByteArray &o)
        : Event(Type), job(j), out(o)
    {}

    Job *job;
    const ByteArray out;
};

#endif
