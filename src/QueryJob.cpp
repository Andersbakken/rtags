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

#include "QueryJob.h"
#include "RTags.h"
#include <rct/EventLoop.h>
#include "Server.h"
#include <regex>
#include "QueryMessage.h"
#include "Project.h"

QueryJob::QueryJob(const std::shared_ptr<QueryMessage> &query,
                   const std::shared_ptr<Project> &proj,
                   Flags<JobFlag> jobFlags)
    : mAborted(false), mLinesWritten(0), mQueryMessage(query), mJobFlags(jobFlags), mProject(proj)
{
    if (mProject)
        mProject->beginScope();
    assert(query);
    if (query->flags() & QueryMessage::SilentQuery)
        setJobFlag(QuietJob);
    const List<String> &pathFilters = query->pathFilters();
    if (!pathFilters.isEmpty()) {
        if (query->flags() & QueryMessage::MatchRegex) {
            const int size = pathFilters.size();
            mPathFiltersRegex.reserve(size);
            for (int i=0; i<size; ++i) {
                mPathFiltersRegex.append(std::regex(pathFilters.at(i).ref()));
            }
        } else {
            mPathFilters = pathFilters;
        }
    }
}

QueryJob::QueryJob(const std::shared_ptr<Project> &proj, Flags<JobFlag> jobFlags)
    : mAborted(false), mLinesWritten(0), mJobFlags(jobFlags), mProject(proj), mPathFilters(0),
      mPathFiltersRegex(0), mConnection(0)
{
    if (mProject)
        mProject->beginScope();
}

QueryJob::~QueryJob()
{
    if (mProject)
        mProject->endScope();
}

uint32_t QueryJob::fileFilter() const
{
    if (mPathFilters.size() == 1) {
        return Location::fileId(mPathFilters.first());
    }
    return 0;
}

bool QueryJob::write(const String &out, Flags<WriteFlag> flags)
{
    if ((mJobFlags & WriteUnfiltered) || (flags & Unfiltered) || filter(out)) {
        if ((mJobFlags & QuoteOutput) && !(flags & DontQuote)) {
            String o((out.size() * 2) + 2, '"');
            char *ch = o.data() + 1;
            int l = 2;
            for (int i=0; i<out.size(); ++i) {
                const char c = out.at(i);
                if (c == '"') {
                    *ch = '\\';
                    ch += 2;
                    l += 2;
                } else {
                    ++l;
                    *ch++ = c;
                }
            }
            o.truncate(l);
            return writeRaw(o, flags);
        } else {
            return writeRaw(out, flags);
        }
    }
    return true;
}

bool QueryJob::writeRaw(const String &out, Flags<WriteFlag> flags)
{
    assert(mConnection);
    if (!(flags & IgnoreMax) && mQueryMessage) {
        const int max = mQueryMessage->max();
        if (max != -1 && mLinesWritten == max) {
            return false;
        }
        assert(mLinesWritten < max || max == -1);
        ++mLinesWritten;
    }

    if (!(mJobFlags & QuietJob))
        error("=> %s", out.constData());

    if (mConnection) {
        if (!mConnection->write(out)) {
            abort();
            return false;
        }
        return true;
    }

    return true;
}

bool QueryJob::write(const Location &location, Flags<WriteFlag> flags)
{
    if (location.isNull())
        return false;
    const int minLine = mQueryMessage ? mQueryMessage->minLine() : -1;
    if (minLine != -1) {
        assert(mQueryMessage);
        assert(mQueryMessage->maxLine() != -1);
        const int maxLine = mQueryMessage->maxLine();
        assert(maxLine != -1);
        const int line = location.line();
        if (line < minLine || line > maxLine) {
            return false;
        }
    }
    if (!(flags & WriteUnfiltered)) {
        const uint32_t filter = fileFilter();
        if (filter) {
            if (filter != location.fileId())
                return false;
            flags |= Unfiltered;
        }
    }
    String out = location.key(keyFlags());
    const bool containingFunction = queryFlags() & QueryMessage::ContainingFunction;
    const bool cursorKind = queryFlags() & QueryMessage::CursorKind;
    const bool displayName = queryFlags() & QueryMessage::DisplayName;
    if (containingFunction || cursorKind || displayName) {
        int idx;
        Symbol symbol = project()->findSymbol(location, &idx);
        if (symbol.isNull()) {
            error() << "Somehow can't find" << location << "in symbols";
        } else {
            if (displayName)
                out += '\t' + symbol.displayName();
            if (cursorKind)
                out += '\t' + symbol.kindSpelling();
            if (containingFunction) {
                const uint32_t fileId = location.fileId();
                const unsigned int line = location.line();
                const unsigned int column = location.column();
                auto fileMap = project()->openSymbols(location.fileId());
                if (fileMap) {
                    while (idx > 0) {
                        symbol = fileMap->valueAt(--idx);
                        if (symbol.location.fileId() != fileId)
                            break;
                        if (symbol.isDefinition()
                            && RTags::isContainer(symbol.kind)
                            && comparePosition(line, column, symbol.startLine, symbol.startColumn) >= 0
                            && comparePosition(line, column, symbol.endLine, symbol.endColumn) <= 0) {
                            out += "\tfunction: " + symbol.symbolName;
                            break;
                        }
                    }
                }
            }
        }
    }
    return write(out, flags);
}

bool QueryJob::write(const Symbol &symbol,
                     Flags<Symbol::ToStringFlag> toStringFlags,
                     Flags<WriteFlag> writeFlags)
{
    if (symbol.isNull())
        return false;

    return write(symbol.toString(toStringFlags, keyFlags(), project()), writeFlags);
}

bool QueryJob::filter(const String &value) const
{
    if (mPathFilters.isEmpty() && mPathFiltersRegex.isEmpty() && !(queryFlags() & QueryMessage::FilterSystemIncludes))
        return true;

    const char *val = value.constData();
    while (*val && isspace(*val))
        ++val;

    if (queryFlags() & QueryMessage::FilterSystemIncludes && Path::isSystem(val))
        return false;

    if (mPathFilters.isEmpty() && mPathFiltersRegex.isEmpty())
        return true;

    assert(mPathFilters.isEmpty() != mPathFiltersRegex.isEmpty());
    String copy;
    const String &ref = (val != value.constData() ? copy : value);
    if (val != value.constData())
        copy = val;
    error() << mPathFilters << ref;
    if (!mPathFilters.isEmpty())
        return RTags::startsWith(mPathFilters, ref);

    assert(!mPathFiltersRegex.isEmpty());

    const int count = mPathFiltersRegex.size();
    for (int i=0; i<count; ++i) {
        if (std::regex_search(ref.constData(), mPathFiltersRegex.at(i)))
            return true;
    }
    return false;
}


int QueryJob::run(const std::shared_ptr<Connection> &connection)
{
    assert(connection);
    mConnection = connection;
    const int ret = execute();
    mConnection = 0;
    return ret;
}
