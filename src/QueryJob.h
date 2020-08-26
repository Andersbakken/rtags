/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef QueryJob_h
#define QueryJob_h

#include <stdarg.h>
#include <stdint.h>
#include <regex>
#include <mutex>
#include <functional>
#include <map>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>

#include "Project.h"
#include "QueryMessage.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/SignalSlot.h"
#include "rct/String.h"
#include "Location.h"
#include "rct/Hash.h"
#include "rct/Path.h"
#include "rct/Set.h"

class Location;
class QueryMessage;
class Connection;
struct Symbol;

class QueryJob : public Project::FileMapScopeScope
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
    virtual ~QueryJob();

    bool hasFilter() const { return mFileFilter || !mFilters.isEmpty(); }
    bool hasKindFilter() const { return !mKindFilters.isEmpty(); }
    bool hasPieceFilter() const { return !mPieceFilters.isEmpty(); }
    List<QueryMessage::PathFilter> pathFilters() const
    {
        if (mQueryMessage)
            return mQueryMessage->pathFilters();
        return List<QueryMessage::PathFilter>();
    }
    Set<String> pieceFilters() const { return mPieceFilters; }
    void setPieceFilters(Set<String> &&pieceFilters) { mPieceFilters = std::move(pieceFilters); }
    uint32_t fileFilter() const { return mFileFilter; }
    enum WriteFlag {
        NoWriteFlags = 0x00,
        IgnoreMax = 0x01,
        DontQuote = 0x02,
        Unfiltered = 0x04,
        NoContext = 0x08
    };
    String symbolToString(const Symbol &symbol) const;

    bool write(const String &out, Flags<WriteFlag> flags = Flags<WriteFlag>());
    bool write(const Symbol &symbol, Flags<WriteFlag> writeFlags = Flags<WriteFlag>());
    bool write(Location location, Flags<WriteFlag> writeFlags = Flags<WriteFlag>());
    enum LocationPiece {
        Piece_Location,
        Piece_Context,
        Piece_SymbolName,
        Piece_Kind,
        Piece_ContainingFunctionName,
        Piece_ContainingFunctionLocation
    };
    bool locationToString(Location location,
                          const std::function<void(LocationPiece, const String &)> &cb,
                          Flags<WriteFlag> writeFlags = Flags<WriteFlag>());

    template <int StaticBufSize>
    bool write(Flags<WriteFlag> writeFlags, const char *format, ...) RCT_PRINTF_WARNING(3, 4);
    template <int StaticBufSize>
    bool write(const char *format, ...) RCT_PRINTF_WARNING(2, 3);
    Flags<JobFlag> jobFlags() const { return mJobFlags; }
    void setJobFlags(Flags<JobFlag> flags) { mJobFlags = flags; }
    void setJobFlag(JobFlag flag, bool on = true) { mJobFlags.set(flag, on); }
    Flags<QueryMessage::Flag> queryFlags() const { return mQueryMessage ? mQueryMessage->flags() : Flags<QueryMessage::Flag>(); }
    std::shared_ptr<QueryMessage> queryMessage() const { return mQueryMessage; }
    Flags<Location::ToStringFlag> locationToStringFlags() const { return QueryMessage::locationToStringFlags(queryFlags()); }
    bool filter(const String &val) const;
    Signal<std::function<void(const String &)> > &output() { return mOutput; }
    std::shared_ptr<Project> project() const { return mProject; }
    virtual int execute() = 0;
    int run(const std::shared_ptr<Connection> &connection = nullptr);
    bool isAborted() const { std::lock_guard<std::mutex> lock(mMutex); return mAborted; }
    void abort() { std::lock_guard<std::mutex> lock(mMutex); mAborted = true; }
    std::mutex &mutex() const { return mMutex; }
    const std::shared_ptr<Connection> &connection() const { return mConnection; }
    bool filterLocation(Location loc) const;
    bool filterKind(const Symbol &symbol) const { return mKindFilters.filter(symbol); }
private:
    class Filter
    {
    public:
        virtual ~Filter() {}
        virtual bool match(uint32_t fileId, const Path &path) const = 0;
    };
    class PathFilter : public Filter
    {
    public:
        PathFilter(const Path &p) : pattern(p) {}
        virtual bool match(uint32_t, const Path &path) const override { return path.startsWith(pattern); }

        const Path pattern;
    };
    class RegexFilter : public Filter
    {
    public:
        RegexFilter(const String &str, bool caseInsensitive) : regex(str.ref(), caseInsensitive ? std::regex::icase : std::regex::ECMAScript) {}
        virtual bool match(uint32_t, const Path &path) const override { return std::regex_search(path.constData(), regex); }

        const std::regex regex;
    };

    class DependencyFilter : public Filter
    {
    public:
        DependencyFilter(uint32_t f, const std::shared_ptr<Project> &p) : fileId(f), project(p) {}
        virtual bool match(uint32_t f, const Path &) const override { return project->dependsOn(fileId, f); }

        const uint32_t fileId;
        const std::shared_ptr<Project> project;
    };

    mutable std::mutex mMutex;
    bool mAborted;
    int mLinesWritten;
    bool writeRaw(const String &out, Flags<WriteFlag> flags);
    std::shared_ptr<QueryMessage> mQueryMessage;
    Flags<JobFlag> mJobFlags;
    Signal<std::function<void(const String &)> > mOutput;
    std::shared_ptr<Project> mProject;
    uint32_t mFileFilter;
    List<std::shared_ptr<Filter> > mFilters;
    QueryMessage::KindFilters mKindFilters;
    Set<String> mPieceFilters;
    String mBuffer;
    std::shared_ptr<Connection> mConnection;
    Hash<Path, String> mContextCache;
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
