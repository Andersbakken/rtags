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
    Job(int id, unsigned flags = 0);
    ~Job();
    void setPathFilters(const List<ByteArray> &filter, bool filterSystemIncludes);
    List<ByteArray> pathFilters() const;
    int id() const { return mId; }
    void write(const ByteArray &out);
    void writeRaw(const ByteArray &out);
    unsigned flags() const { return mFlags; }
    void setFlags(unsigned flags) { mFlags = flags; }
    bool filter(const ByteArray &val) const;
    virtual void run();
    virtual void execute() {}
    signalslot::Signal1<const ByteArray &> &output() { return mOutput; }
    void write(const Location &location, const CursorInfo &info);
private:
    const int mId;
    unsigned mFlags;
    List<ByteArray> mPathFilters;
    bool mFilterSystemIncludes;
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
