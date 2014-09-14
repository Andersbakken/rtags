/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef Job_h
#define Job_h

#include <rct/ThreadPool.h>
#include <rct/List.h>
#include <rct/String.h>
#include <rct/EventLoop.h>
#include <rct/SignalSlot.h>
#include <rct/RegExp.h>
#include "RTagsClang.h"
#include "QueryMessage.h"
#include <mutex>

class CursorInfo;
class Location;
class QueryMessage;
class Project;
class Connection;
class QueryJob
{
public:
    enum Flag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2,
        QuietJob = 0x4
    };
    enum { Priority = 10 };
    QueryJob(const std::shared_ptr<QueryMessage> &msg, unsigned jobFlags, const std::shared_ptr<Project> &proj);
    QueryJob(unsigned jobFlags, const std::shared_ptr<Project> &project);
    ~QueryJob();

    bool hasFilter() const { return mPathFilters || mPathFiltersRegExp; }
    List<String> pathFilters() const { return mPathFilters ? *mPathFilters : List<String>(); }
    uint32_t fileFilter() const;
    enum WriteFlag {
        NoWriteFlags = 0x0,
        IgnoreMax = 0x1,
        DontQuote = 0x2,
        Unfiltered = 0x4
    };
    bool write(const String &out, unsigned flags = NoWriteFlags);
    bool write(const std::shared_ptr<CursorInfo> &info, unsigned flags = NoWriteFlags);
    bool write(const Location &location, unsigned flags = NoWriteFlags);

    template <int StaticBufSize> bool write(unsigned flags, const char *format, ...);
    template <int StaticBufSize> bool write(const char *format, ...);
    unsigned jobFlags() const { return mJobFlags; }
    void setJobFlags(unsigned flags) { mJobFlags = flags; }
    void setJobFlag(Flag flag, bool on = true) { if (on) { mJobFlags |= flag; } else { mJobFlags &= ~flag; } }
    unsigned queryFlags() const { return mQueryMessage ? mQueryMessage->flags() : 0; }
    unsigned keyFlags() const { return QueryMessage::keyFlags(queryFlags()); }
    bool filter(const String &val) const;
    Signal<std::function<void(const String &)> > &output() { return mOutput; }
    std::shared_ptr<Project> project() const { return mProject.lock(); }
    virtual int execute() = 0;
    int run(Connection *connection = 0);
    bool isAborted() const { std::lock_guard<std::mutex> lock(mMutex); return mAborted; }
    void abort() { std::lock_guard<std::mutex> lock(mMutex); mAborted = true; }
    std::mutex &mutex() const { return mMutex; }
    bool &aborted() { return mAborted; }
    Connection *connection() const { return mConnection; }
private:
    mutable std::mutex mMutex;
    bool mAborted;
    int mLinesWritten;
    bool writeRaw(const String &out, unsigned flags);
    std::shared_ptr<QueryMessage> mQueryMessage;
    unsigned mJobFlags;
    Signal<std::function<void(const String &)> > mOutput;
    std::weak_ptr<Project> mProject;
    List<String> *mPathFilters;
    List<RegExp> *mPathFiltersRegExp;
    String mBuffer;
    Connection *mConnection;
};

template <int StaticBufSize>
inline bool QueryJob::write(unsigned flags, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const String ret = String::format<StaticBufSize>(format, args);
    va_end(args);
    return write(ret, flags);
}

template <int StaticBufSize>
inline bool QueryJob::write(const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const String ret = String::format<StaticBufSize>(format, args);
    va_end(args);
    return write(ret);
}

#endif
