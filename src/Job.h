#ifndef Job_h
#define Job_h

#include <rct/ThreadPool.h>
#include <rct/List.h>
#include <rct/String.h>
#include <rct/Event.h>
#include <rct/SignalSlot.h>
#include "Server.h"
#include <rct/RegExp.h>
#include "RTagsClang.h"

class CursorInfo;
class Location;
class QueryMessage;
class Project;
class Job : public ThreadPool::Job, public enable_shared_from_this<Job>
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
    bool write(const String &out, unsigned flags = NoWriteFlags);
    bool write(const CursorInfo &info, unsigned flags = NoWriteFlags);
    bool write(const Location &location, unsigned flags = NoWriteFlags);

    template <int StaticBufSize> bool write(unsigned flags, const char *format, ...);
    template <int StaticBufSize> bool write(const char *format, ...);
    unsigned jobFlags() const { return mJobFlags; }
    void setJobFlags(unsigned flags) { mJobFlags = flags; }
    unsigned queryFlags() const { return mQueryFlags; }
    void setQueryFlags(unsigned queryFlags) { mQueryFlags = queryFlags; }
    unsigned keyFlags() const;
    inline bool filter(const String &val) const;
    signalslot::Signal1<const String &> &output() { return mOutput; }
    shared_ptr<Project> project() const { return mProject.lock(); }
    virtual void run();
    virtual void execute() = 0;
    void run(Connection *connection);
    bool isAborted() const { MutexLocker lock(&mMutex); return mAborted; }
    void abort() { MutexLocker lock(&mMutex); mAborted = true; }
protected:
    mutable Mutex mMutex;
    bool mAborted;
private:
    bool writeRaw(const String &out, unsigned flags);
    int mId, mMinOffset, mMaxOffset;
    unsigned mJobFlags;
    unsigned mQueryFlags;
    signalslot::Signal1<const String &> mOutput;
    weak_ptr<Project> mProject;
    List<String> *mPathFilters;
    List<RegExp> *mPathFiltersRegExp;
    int mMax;
    String mBuffer;
    Connection *mConnection;
};

template <int StaticBufSize>
inline bool Job::write(unsigned flags, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const String ret = String::format<StaticBufSize>(format, args);
    va_end(args);
    return write(ret, flags);
}

template <int StaticBufSize>
inline bool Job::write(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const String ret = String::format<StaticBufSize>(format, args);
    va_end(args);
    return write(ret);
}

inline bool Job::filter(const String &value) const
{
    if (!mPathFilters && !mPathFiltersRegExp && !(mQueryFlags & QueryMessage::FilterSystemIncludes))
        return true;

    const char *val = value.constData();
    while (*val && isspace(*val))
        ++val;

    if (mQueryFlags & QueryMessage::FilterSystemIncludes && Path::isSystem(val))
        return false;

    if (!mPathFilters && !mPathFiltersRegExp)
        return true;
    
    assert(!mPathFilters != !mPathFiltersRegExp);
    String copy;
    const String &ref = (val != value.constData() ? copy : value);
    if (val != value.constData())
        copy = val;
    if (mPathFilters)
        return RTags::startsWith(*mPathFilters, ref);

    assert(mPathFiltersRegExp);

    const int count = mPathFiltersRegExp->size();
    for (int i=0; i<count; ++i) {
        if (mPathFiltersRegExp->at(i).indexIn(ref) != -1)
            return true;
    }
    return false;
}

class JobOutputEvent : public Event
{
public:
    enum { Type = 2 };
    JobOutputEvent(const shared_ptr<Job> &j, const String &o, bool f)
        : Event(Type), job(j), out(o), finish(f), id(j->id())
    {
    }

    weak_ptr<Job> job;
    const String out;
    const bool finish;
    const int id;
};

#endif
