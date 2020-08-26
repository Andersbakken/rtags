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

#include "QueryJob.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "Project.h"
#include "QueryMessage.h"
#include "RTags.h"
#include "rct/Connection.h"
#include "FileMap.h"
#include "Symbol.h"
#include "rct/Log.h"
#include "rct/Value.h"

QueryJob::QueryJob(const std::shared_ptr<QueryMessage> &query,
                   const std::shared_ptr<Project> &proj,
                   Flags<JobFlag> jobFlags)
    : Project::FileMapScopeScope(proj), mAborted(false), mLinesWritten(0), mQueryMessage(query), mJobFlags(jobFlags), mProject(proj), mFileFilter(0)
{
    assert(query);
    if (query->flags() & QueryMessage::SilentQuery)
        setJobFlag(QuietJob);
    const List<QueryMessage::PathFilter> &pathFilters = query->pathFilters();
    if (!pathFilters.isEmpty()) {
        if (pathFilters.size() == 1 && pathFilters.first().mode == QueryMessage::PathFilter::Self) {
            mFileFilter = Location::fileId(pathFilters.first().pattern);
        }
        if (!mFileFilter) {
            for (const QueryMessage::PathFilter &filter : pathFilters) {
                if (filter.mode == QueryMessage::PathFilter::Dependency) {
                    uint32_t f = Location::fileId(filter.pattern);
                    if (f && mProject)
                        mFilters.append(std::make_shared<DependencyFilter>(f, mProject));
                } else if (query->flags() & QueryMessage::MatchRegex) {
                    mFilters.append(std::make_shared<RegexFilter>(filter.pattern, query->flags() & QueryMessage::MatchCaseInsensitive));
                } else {
                    mFilters.append(std::make_shared<PathFilter>(filter.pattern));
                }
            }
        }
    }
    mKindFilters = query->kindFilters();
}

QueryJob::~QueryJob()
{
}

bool QueryJob::write(const String &out, Flags<WriteFlag> flags)
{
    if ((mJobFlags & WriteUnfiltered) || (flags & Unfiltered) || filter(out)) {
        if ((mJobFlags & QuoteOutput) && !(flags & DontQuote)) {
            String o((out.size() * 2) + 2, '"');
            char *ch = o.data() + 1;
            int l = 2;
            for (size_t i = 0; i < out.size(); ++i) {
                const char c = out.at(i);
                switch (c) {
                case '"':
                case '\\':
                    *(ch + 1) = c;
                    *ch = '\\';
                    ch += 2;
                    l += 2;
                    break;
                default:
                    ++l;
                    *ch++ = c;
                    break;
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
        warning("=> %s", out.constData());

    if (mConnection) {
        if (!mConnection->write(out)) {
            abort();
            return false;
        }
        return true;
    }

    return true;
}

bool QueryJob::locationToString(Location location,
                                const std::function<void(LocationPiece, const String &)> &cb,
                                Flags<WriteFlag>
                                writeFlags)
{
    if (location.isNull())
        return false;
    Flags<Location::ToStringFlag> kf = locationToStringFlags();
    kf &= ~Location::ShowContext;
    cb(Piece_Location, location.toString(kf, &mContextCache));
    if (!(writeFlags & NoContext) && !(queryFlags() & QueryMessage::NoContext))
        cb(Piece_Context, location.context(kf, &mContextCache));

    const bool containingFunction = queryFlags() & QueryMessage::ContainingFunction;
    const bool containingFunctionLocation = queryFlags() & QueryMessage::ContainingFunctionLocation;
    const bool cursorKind = queryFlags() & QueryMessage::CursorKind;
    const bool displayName = queryFlags() & QueryMessage::DisplayName;
    if (containingFunction || containingFunctionLocation || cursorKind || displayName || !mKindFilters.isEmpty()) {
        int idx;
        Symbol symbol = project()->findSymbol(location, &idx);
        if (symbol.isNull()) {
            if (!(symbol.flags & Symbol::FileSymbol)) {
                error() << "Somehow can't find" << location << "in symbols";
            }
        } else {
            if (!mKindFilters.filter(symbol))
                return false;
            if (displayName)
                cb(Piece_SymbolName, symbol.displayName());
            if (cursorKind)
                cb(Piece_Kind, symbol.kindSpelling());
            if (containingFunction || containingFunctionLocation) {
                const uint32_t fileId = location.fileId();
                const unsigned int line = location.line();
                const unsigned int column = location.column();
                auto fileMap = project()->openSymbols(location.fileId());
                if (fileMap) {
                    while (idx > 0) {
                        symbol = fileMap->valueAt(--idx);
                        if (symbol.location.fileId() != fileId)
                            break;
                        if (symbol.isDefinition() && RTags::isContainer(symbol.kind)
                            && comparePosition(line, column, symbol.startLine, symbol.startColumn) >= 0
                            && comparePosition(line, column, symbol.endLine, symbol.endColumn) <= 0) {
                            if (containingFunction)
                                cb(Piece_ContainingFunctionName, symbol.symbolName);
                            if (containingFunctionLocation)
                                cb(Piece_ContainingFunctionLocation,
                                   symbol.location.toString(locationToStringFlags() & ~Location::ShowContext));
                            break;
                        }
                    }
                }
            }
        }
    }
    return true;
}

bool QueryJob::write(Location location, Flags<WriteFlag> flags)
{
    if (location.isNull())
        return false;
    if (!(flags & Unfiltered)) {
        if (!filterLocation(location))
            return false;
        flags |= Unfiltered;
    }

    String out;
    if (!locationToString(location,
                          [&out](LocationPiece piece, const String &string) {
                              switch (piece) {
                              case Piece_Location:
                                  break;
                              case Piece_SymbolName:
                              case Piece_Kind:
                              case Piece_Context:
                                  out << '\t';
                                  break;
                              case Piece_ContainingFunctionName:
                              case Piece_ContainingFunctionLocation:
                                  out << "\tfunction: ";
                                  break;
                              }
                              out << string;
                          },
                          flags))
        return false;
    return write(out, flags);
}

String QueryJob::symbolToString(const Symbol &symbol) const
{
    Flags<Symbol::ToStringFlag> toStringFlags;
    if (queryFlags() & QueryMessage::SymbolInfoIncludeTargets)
        toStringFlags |= Symbol::IncludeTargets;
    if (queryFlags() & QueryMessage::SymbolInfoIncludeReferences)
        toStringFlags |= Symbol::IncludeReferences;
    if (queryFlags() & QueryMessage::SymbolInfoIncludeParents)
        toStringFlags |= Symbol::IncludeParents;
    if (queryFlags() & QueryMessage::SymbolInfoIncludeBaseClasses)
        toStringFlags |= Symbol::IncludeBaseClasses;
    if (queryFlags() & QueryMessage::SymbolInfoIncludeSourceCode)
        toStringFlags |= Symbol::IncludeSourceCode;
    if (queryFlags() & (QueryMessage::ContainingFunction | QueryMessage::JSON | QueryMessage::Elisp))
        toStringFlags |= Symbol::IncludeContainingFunction;
    if (queryFlags() & (QueryMessage::ContainingFunctionLocation | QueryMessage::JSON | QueryMessage::Elisp))
        toStringFlags |= Symbol::IncludeContainingFunctionLocation;

    if (symbol.isNull()) {
        return String();
    }

    if (!filterLocation(symbol.location)) {
        return String();
    }

    if (!mKindFilters.filter(symbol)) {
        return String();
    }

    String out;
    if (queryFlags() & (QueryMessage::Elisp | QueryMessage::JSON)) {
        Value val = symbol.toValue(project(), toStringFlags, locationToStringFlags() | Location::NoColor, mPieceFilters);
        if (queryFlags() & QueryMessage::Elisp) {
            out = RTags::toElisp(val);
        } else {
            out = val.toJSON();
        }
    } else {
        out = symbol.toString(project(), toStringFlags, locationToStringFlags(), mPieceFilters);
    }
    return out;
}

bool QueryJob::write(const Symbol &symbol, Flags<WriteFlag> writeFlags)
{
    String out = symbolToString(symbol);
    if (!out.isEmpty())
        return write(out, writeFlags | Unfiltered);
    return true;
}

bool QueryJob::filter(const String &value) const
{
    if (mFilters.isEmpty() && !(queryFlags() & QueryMessage::FilterSystemIncludes))
        return true;

    const char *val = value.constData();
    while (*val && isspace(*val))
        ++val;
    const char *space = strchr(val, ' ');
    Path path;
    uint32_t fileId = 0;
    if (space) {
        path.assign(val, space - val);
    } else {
        path = val;
    }

    if (!path.isFile())
        return true; // non-file things go unfiltered

    if (queryFlags() & QueryMessage::FilterSystemIncludes && Path::isSystem(val))
        return false;
    fileId = Location::fileId(path);

    if (mFilters.isEmpty())
        return true;

    for (const std::shared_ptr<Filter> &filter : mFilters) {
        if (filter->match(fileId, path))
            return true;
    }
    return false;
}

int QueryJob::run(const std::shared_ptr<Connection> &connection)
{
    assert(connection);
    mConnection = connection;
    const int ret = execute();
    mConnection = nullptr;
    return ret;
}

bool QueryJob::filterLocation(Location loc) const
{
    if (mFileFilter && loc.fileId() != mFileFilter)
        return false;
    const int minLine = mQueryMessage ? mQueryMessage->minLine() : -1;
    if (minLine != -1) {
        assert(mQueryMessage);
        assert(mQueryMessage->maxLine() != -1);
        const int maxLine = mQueryMessage->maxLine();
        assert(maxLine != -1);
        const int line = loc.line();
        if (line < minLine || line > maxLine) {
            return false;
        }
    }
    if (!mFilters.isEmpty()) {
        for (const std::shared_ptr<Filter> &filter : mFilters) {
            if (filter->match(loc.fileId(), loc.path()))
                return true;
        }
        return false;
    }
    return true;
}
