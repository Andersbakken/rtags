/* This file is part of RTags (http://rtags.net).

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

#ifndef QueryJob_h
#define QueryJob_h

#include <rct/ThreadPool.h>
#include <rct/List.h>
#include <rct/String.h>
#include <rct/EventLoop.h>
#include <rct/SignalSlot.h>
#include <regex>
#include "RTagsClang.h"
#include "QueryMessage.h"
#include <mutex>
#include <rct/Flags.h>

class Location;
class QueryMessage;
class Project;
class Connection;
struct Symbol;
class QueryJob
{
public:
    enum JobFlag {
        None = 0x0,
        WriteUnfiltered = 0x1,
        QuoteOutput = 0x2,
        QuietJob = 0x4
    };
    enum { Priority = 10 };
    QueryJob(const std::shared_ptr<QueryMessage> &msg,
             const std::shared_ptr<Project> &proj,
             Flags<JobFlag> jobFlags = Flags<JobFlag>());
    QueryJob(const std::shared_ptr<Project> &project,
             Flags<JobFlag> jobFlags = Flags<JobFlag>());
    ~QueryJob();

    bool hasFilter() const { return !mPathFilters.isEmpty() || !mPathFiltersRegex.isEmpty(); }
    List<String> pathFilters() const { return mPathFilters; }
    uint32_t fileFilter() const;
    enum WriteFlag {
        NoWriteFlags = 0x0,
        IgnoreMax = 0x1,
        DontQuote = 0x2,
        Unfiltered = 0x4
    };
    bool write(const String &out, Flags<WriteFlag> flags = Flags<WriteFlag>());
    bool write(const Symbol &symbol,
               Flags<Symbol::ToStringFlag> sourceFlags = Flags<Symbol::ToStringFlag>(),
               Flags<WriteFlag> writeFlags = Flags<WriteFlag>());
    bool write(const Location &location, Flags<WriteFlag> writeFlags = Flags<WriteFlag>());

    template <int StaticBufSize> bool write(Flags<WriteFlag> writeFlags, const char *format, ...);
    template <int StaticBufSize> bool write(const char *format, ...);
    Flags<JobFlag> jobFlags() const { return mJobFlags; }
    void setJobFlags(Flags<JobFlag> flags) { mJobFlags = flags; }
    void setJobFlag(JobFlag flag, bool on = true) { mJobFlags.set(flag, on); }
    Flags<QueryMessage::Flag> queryFlags() const { return mQueryMessage ? mQueryMessage->flags() : Flags<QueryMessage::Flag>(); }
    std::shared_ptr<QueryMessage> queryMessage() const { return mQueryMessage; }
    Flags<Location::KeyFlag> keyFlags() const { return QueryMessage::keyFlags(queryFlags()); }
    bool filter(const String &val) const;
    Signal<std::function<void(const String &)> > &output() { return mOutput; }
    std::shared_ptr<Project> project() const { return mProject; }
    virtual int execute() = 0;
    int run(const std::shared_ptr<Connection> &connection = 0);
    bool isAborted() const { std::lock_guard<std::mutex> lock(mMutex); return mAborted; }
    void abort() { std::lock_guard<std::mutex> lock(mMutex); mAborted = true; }
    std::mutex &mutex() const { return mMutex; }
    const std::shared_ptr<Connection> &connection() const { return mConnection; }
private:
    mutable std::mutex mMutex;
    bool mAborted;
    int mLinesWritten;
    bool writeRaw(const String &out, Flags<WriteFlag> flags);
    std::shared_ptr<QueryMessage> mQueryMessage;
    Flags<JobFlag> mJobFlags;
    Signal<std::function<void(const String &)> > mOutput;
    std::shared_ptr<Project> mProject;
    List<String> mPathFilters;
    List<std::regex> mPathFiltersRegex;
    String mBuffer;
    std::shared_ptr<Connection> mConnection;
};

RCT_FLAGS(QueryJob::JobFlag);
RCT_FLAGS(QueryJob::WriteFlag);

template <int StaticBufSize>
inline bool QueryJob::write(Flags<WriteFlag> flags, const char *format, ...)
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
