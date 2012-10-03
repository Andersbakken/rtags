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
#include <RTagsClang.h>

class CursorInfo;
class Location;
class QueryMessage;
class Project;
class Job : public ThreadPool::Job, public AbortInterface,
            public enable_shared_from_this<Job>
{
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2,
        WriteBuffered = 0x4
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
        IgnoreMax = 0x1,
        DontQuote = 0x2
    };
    bool write(const ByteArray &out, unsigned flags = NoWriteFlags);
    bool write(const Location &location, const CursorInfo &info, unsigned flags = NoWriteFlags);
    template <int StaticBufSize> bool write(unsigned flags, const char *format, ...);
    template <int StaticBufSize> bool write(const char *format, ...);
    unsigned jobFlags() const { return mJobFlags; }
    void setJobFlags(unsigned flags) { mJobFlags = flags; }
    unsigned queryFlags() const { return mQueryFlags; }
    void setQueryFlags(unsigned queryFlags) { mQueryFlags = queryFlags; }
    unsigned keyFlags() const;
    inline bool filter(const ByteArray &val) const;
    signalslot::Signal1<const ByteArray &> &output() { return mOutput; }
    shared_ptr<Project> project() const { return mProject; }
    void resetProject() { mProject.reset(); }
    virtual void run();
    virtual void execute() = 0;
private:
    bool writeRaw(const ByteArray &out, unsigned flags);
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

template <int StaticBufSize>
inline bool Job::write(unsigned flags, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const ByteArray ret = ByteArray::snprintf<StaticBufSize>(format, args);
    va_end(args);
    return write(ret, flags);
}

template <int StaticBufSize>
inline bool Job::write(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const ByteArray ret = ByteArray::snprintf<StaticBufSize>(format, args);
    va_end(args);
    return write(ret);
}

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
    JobOutputEvent(const shared_ptr<Job> &j, const ByteArray &o, bool f)
        : Event(Type), job(j), out(o), finish(f), id(j->id())
    {}

    weak_ptr<Job> job;
    const ByteArray out;
    const bool finish;
    const int id;
};

#endif
