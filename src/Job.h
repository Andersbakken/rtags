#ifndef Job_h
#define Job_h

#include <rct/ThreadPool.h>
#include <rct/List.h>
#include <rct/String.h>
#include <rct/EventLoop.h>
#include <rct/SignalSlot.h>
#include <rct/RegExp.h>
#include "RTagsClang.h"
#include <mutex>

class CursorInfo;
class Location;
class QueryMessage;
class Project;
class Connection;
class Job : public ThreadPool::Job, public enable_shared_from_this<Job>
{
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2,
        WriteBuffered = 0x4,
        QuietJob = 0x8
    };
    enum { Priority = 10 };
    Job(const QueryMessage &msg, unsigned jobFlags, const shared_ptr<Project> &proj);
    Job(unsigned jobFlags, const shared_ptr<Project> &project);
    ~Job();

    bool hasFilter() const { return mPathFilters || mPathFiltersRegExp; }
    List<String> pathFilters() const { return mPathFilters ? *mPathFilters : List<String>(); }
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
    bool filter(const String &val) const;
    Signal<std::function<void(const String &)> > &output() { return mOutput; }
    shared_ptr<Project> project() const { return mProject.lock(); }
    virtual void run();
    virtual void execute() = 0;
    void run(Connection *connection);
    bool isAborted() const { std::lock_guard<std::mutex> lock(mMutex); return mAborted; }
    void abort() { std::lock_guard<std::mutex> lock(mMutex); mAborted = true; }
    String context() const { return mContext; }
    std::mutex &mutex() const { return mMutex; }
    bool &aborted() { return mAborted; }
private:
    mutable std::mutex mMutex;
    bool mAborted;
    bool writeRaw(const String &out, unsigned flags);
    int mId, mMinOffset, mMaxOffset;
    unsigned mJobFlags;
    unsigned mQueryFlags;
    Signal<std::function<void(const String &)> > mOutput;
    weak_ptr<Project> mProject;
    List<String> *mPathFilters;
    List<RegExp> *mPathFiltersRegExp;
    int mMax;
    String mBuffer;
    Connection *mConnection;
    const String mContext;
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

class JobOutput
{
public:
    JobOutput(const shared_ptr<Job> &j, const String &o, bool f)
        : job(j), out(o), finish(f), id(j->id())
    {
    }

    weak_ptr<Job> job;
    const String out;
    const bool finish;
    const int id;
};

#endif
