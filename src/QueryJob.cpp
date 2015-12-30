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

#include <regex>

#include "Project.h"
#include "QueryMessage.h"
#include "rct/Connection.h"
#include "rct/EventLoop.h"
#include "RTags.h"
#include "Server.h"

QueryJob::QueryJob(const std::shared_ptr<QueryMessage> &query,
                   const std::shared_ptr<Project> &proj,
                   Flags<JobFlag> jobFlags)
    : mAborted(false), mLinesWritten(0), mQueryMessage(query), mJobFlags(jobFlags), mProject(proj), mFileFilter(0)
{
    if (mProject)
        mProject->beginScope();
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
                        mFilters.append(std::shared_ptr<Filter>(new DependencyFilter(f, mProject)));
                } else if (query->flags() & QueryMessage::MatchRegex) {
                    mFilters.append(std::shared_ptr<Filter>(new RegexFilter(filter.pattern)));
                } else {
                    mFilters.append(std::shared_ptr<Filter>(new PathFilter(filter.pattern)));
                }
            }
        }
    }
    mKindFilters = query->kindFilters();
}

QueryJob::~QueryJob()
{
    if (mProject)
        mProject->endScope();
}

bool QueryJob::write(const String &out, Flags<WriteFlag> flags)
{
    if ((mJobFlags & WriteUnfiltered) || (flags & Unfiltered) || filter(out)) {
        if ((mJobFlags & QuoteOutput) && !(flags & DontQuote)) {
            String o((out.size() * 2) + 2, '"');
            char *ch = o.data() + 1;
            int l = 2;
            for (size_t i=0; i<out.size(); ++i) {
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

bool QueryJob::locationToString(const Location &location,
                                const std::function<void(LocationPiece, const String &)> &cb,
                                Flags<WriteFlag> writeFlags)
{
    if (location.isNull())
        return false;
    Flags<Location::ToStringFlag> kf = locationToStringFlags();
    kf &= ~Location::ShowContext;
    cb(Piece_Location, location.toString(kf));
    if (!(writeFlags & NoContext) && !(queryFlags() & QueryMessage::NoContext))
        cb(Piece_Context, location.context(kf));

    const bool containingFunction = queryFlags() & QueryMessage::ContainingFunction;
    const bool containingFunctionLocation = queryFlags() & QueryMessage::ContainingFunctionLocation;
    const bool cursorKind = queryFlags() & QueryMessage::CursorKind;
    const bool displayName = queryFlags() & QueryMessage::DisplayName;
    if (containingFunction || containingFunctionLocation || cursorKind || displayName || !mKindFilters.isEmpty()) {
        int idx;
        Symbol symbol = project()->findSymbol(location, &idx);
        if (symbol.isNull()) {
            error() << "Somehow can't find" << location << "in symbols";
        } else {
            if (!filterKind(symbol.kind))
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
                        if (symbol.isDefinition()
                            && RTags::isContainer(symbol.kind)
                            && comparePosition(line, column, symbol.startLine, symbol.startColumn) >= 0
                            && comparePosition(line, column, symbol.endLine, symbol.endColumn) <= 0) {
                            if (containingFunction)
                                cb(Piece_ContainingFunctionName, symbol.symbolName);
                            if (containingFunctionLocation)
                                cb(Piece_ContainingFunctionLocation, symbol.location.toString(locationToStringFlags() & ~Location::ShowContext));
                            break;
                        }
                    }
                }
            }
        }
    }
    return true;
}

bool QueryJob::write(const Location &location, Flags<WriteFlag> flags)
{
    if (location.isNull())
        return false;
    if (!(flags & Unfiltered)) {
        if (!filterLocation(location))
            return false;
        flags |= Unfiltered;
    }

    String out;
    if (!locationToString(location, [&out](LocationPiece piece, const String &string) {
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
            }, flags))
        return false;
    return write(out, flags);
}

enum ToStringFlag {
    None = 0x0,
    Indent = 0x1,
    Quote = 0x2,
    NoQuote = 0x4,
    ElispEscape = 0x8
};

RCT_FLAGS(ToStringFlag);

template <typename T>
static inline void toString(String &out, const T &t, Flags<ToStringFlag> flags)
{
    if (flags & Quote)
        out << '"';
    out << t;
    if (flags & Quote)
        out << '"';
}

template <>
inline void toString(String &out, const String &string, Flags<ToStringFlag> flags)
{
    if (!(flags & NoQuote))
        out << '"';
    if (flags & ElispEscape) {
        out << RTags::elispEscape(string);
    } else {
       out << string;
    }
    if (!(flags & NoQuote))
        out << '"';
}

template <>
inline void toString(String &out, const bool &b, Flags<ToStringFlag>)
{
    out << (b ? "t" : "nil");
}

template <>
inline void toString(String &out, const Location &loc, Flags<ToStringFlag> flags)
{
    toString(out, loc.toString(Location::NoColor|Location::AbsolutePath), flags);
}

template <typename T>
inline static void toString(String &out, const List<T> &list, Flags<ToStringFlag> flags)
{
    assert(!list.isEmpty());
    out << "(list";
    for (const T &t : list) {
        out << ' ';
        if (flags & Indent)
            out << ' ';
        toString(out, t, flags);
    }
    out << ")";
}

template <typename T>
inline static void elisp(String &out, const char *name, const T &t, Flags<ToStringFlag> flags = None)
{
    if (flags & Indent)
        out << ' ';
    out << " (cons '" << name << ' ';
    toString(out, t, flags);
    out << ")\n";
}

bool QueryJob::write(const Symbol &symbol,
                     Flags<Symbol::ToStringFlag> toStringFlags,
                     Flags<WriteFlag> writeFlags)
{
    if (symbol.isNull())
        return false;

    if (!filterLocation(symbol.location))
        return false;

    if (!filterKind(symbol.kind))
        return false;

    if (queryFlags() & QueryMessage::Elisp) {
        enum SymbolToElispMode {
            Mode_Symbol,
            Mode_Target,
            Mode_Parent,
            Mode_BaseClass,
            Mode_Reference,
            Mode_Argument
        };
        std::function<String(const Symbol &, SymbolToElispMode)> symbolToElisp = [&](const Symbol &symbol, SymbolToElispMode mode) {
            String out;
            out.reserve(1024);
            if (mode != Mode_Symbol)
                out << "\n ";
            out << "(list\n";
            Flags<ToStringFlag> flags;
            if (mode != Mode_Symbol)
                flags |= Indent;
            elisp(out, "location", symbol.location, flags);
            elisp(out, "symbolName", symbol.symbolName, flags);
            elisp(out, "usr", symbol.usr, flags);
            if (!symbol.baseClasses.isEmpty()) {
                List<String> baseClasses;
                for (const auto &base : symbol.baseClasses) {
                    Symbol sym;
                    if (mode == Mode_Symbol) {
                        for (const Symbol &s : project()->findByUsr(base, symbol.location.fileId(), Project::ArgDependsOn, symbol.location)) {
                            sym = s;
                            break;
                        }
                    }
                    if (!sym.isNull()) {
                        baseClasses << symbolToElisp(sym, Mode_BaseClass);
                    }
                }

                if (!baseClasses.isEmpty())
                    elisp(out, "baseClasses", baseClasses, flags | NoQuote);
            }
            if (!symbol.arguments.isEmpty()) {
                List<String> arguments;
                for (const auto &arg : symbol.arguments) {
                    Symbol sym;
                    if (mode == Mode_Symbol)
                        sym = project()->findSymbol(arg);
                    if (!sym.isNull()) {
                        arguments << symbolToElisp(sym, Mode_Argument);
                    }
                }

                if (!arguments.isEmpty())
                    elisp(out, "arguments", arguments, flags | NoQuote);
            }

            elisp(out, "symbolLength", static_cast<uint32_t>(symbol.symbolLength), flags);
            elisp(out, "kind", symbol.kind, flags | Quote);
            if (!symbol.typeName.isEmpty()) {
                elisp(out, "type", symbol.typeName, flags);
            } else if (symbol.type != CXType_Invalid) {
                elisp(out, "type", symbol.type, flags | Quote);
            }
            elisp(out, "linkage", symbol.linkage, flags | Quote);
            elisp(out, "briefComment", symbol.briefComment, flags | ElispEscape);
            elisp(out, "xmlComment", symbol.xmlComment, flags | ElispEscape);
            elisp(out, "startLine", symbol.startLine, flags);
            elisp(out, "startColumn", symbol.startColumn, flags);
            elisp(out, "endLine", symbol.endLine, flags);
            elisp(out, "endColumn", symbol.endColumn, flags);
            if (symbol.size > 0)
                elisp(out, "sizeof", symbol.size, flags);
            if (symbol.fieldOffset > 0)
                elisp(out, "fieldOffset", symbol.fieldOffset, flags);
            if (symbol.alignment > 0)
                elisp(out, "alignment", symbol.alignment, flags);
            if (symbol.kind == CXCursor_EnumConstantDecl)
                elisp(out, "enumValue", symbol.enumValue, flags);
            if (symbol.isDefinition()) {
                elisp(out, "definition", true, flags);
            } else if (symbol.isReference()) {
                elisp(out, "reference", true, flags);
            }
            if (symbol.isContainer())
                elisp(out, "container", true, flags);
            if ((symbol.flags & Symbol::PureVirtualMethod) == Symbol::PureVirtualMethod)
                elisp(out, "PureVirtual", true, flags);
            if (symbol.flags & Symbol::VirtualMethod)
                elisp(out, "Virtual", true, flags);
            if (symbol.flags & Symbol::ConstMethod)
                elisp(out, "ConstMethod", true, flags);
            if (symbol.flags & Symbol::StaticMethod)
                elisp(out, "StaticMethod", true, flags);
            if (symbol.flags & Symbol::Variadic)
                elisp(out, "Variadic", true, flags);
            if (symbol.flags & Symbol::Auto)
                elisp(out, "Auto", true, flags);
            if (symbol.flags & Symbol::AutoRef)
                elisp(out, "AutoRef", true, flags);
            if (symbol.flags & Symbol::MacroExpansion)
                elisp(out, "MacroExpansion", true, flags);
            if (symbol.flags & Symbol::TemplateSpecialization)
                elisp(out, "TemplateSpecialization", true, flags);
            switch (mode) {
            case Mode_Symbol: {
                const auto targets = project()->findTargets(symbol);
                if (targets.size()) {
                    List<String> t;
                    for (const auto &target : targets) {
                        t << symbolToElisp(target, Mode_Target);
                    }
                    elisp(out, "targets", t, flags | NoQuote);
                }
                const auto references = project()->findTargets(symbol);
                if (references.size()) {
                    List<String> r;
                    for (const auto &reference : references) {
                        r << symbolToElisp(reference, Mode_Reference);
                    }
                    elisp(out, "references", r, flags | NoQuote);
                }}
                // fall through
            case Mode_Parent: {
                auto syms = project()->openSymbols(symbol.location.fileId());
                int idx = -1;
                if (syms) {
                    idx = syms->lowerBound(symbol.location);
                    if (idx == -1) {
                        idx = syms->count() - 1;
                    }
                }
                const unsigned int line = symbol.location.line();
                const unsigned int column = symbol.location.column();
                while (idx-- > 0) {
                    const Symbol s = syms->valueAt(idx);
                    if (s.isDefinition()
                        && s.isContainer()
                        && comparePosition(line, column, s.startLine, s.startColumn) >= 0
                        && comparePosition(line, column, s.endLine, s.endColumn) <= 0) {
                        const String p = symbolToElisp(s, Mode_Parent);
                        elisp(out, "parent", p, flags | NoQuote);
                        break;
                    }
                }
                break; }
            case Mode_Reference:
            case Mode_BaseClass:
            case Mode_Argument:
                break;
            case Mode_Target:
                elisp(out, "targetRank", RTags::targetRank(symbol.kind), flags);
                break;
            }
            out.chop(1);
            out << ')';
            return out;
        };
        const String out = symbolToElisp(symbol, Mode_Symbol);
        return write(out, writeFlags|Unfiltered);
    } else {
        return write(symbol.toString(toStringFlags, locationToStringFlags(), project()), writeFlags|Unfiltered);
    }
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
    mConnection = 0;
    return ret;
}

bool QueryJob::filterLocation(const Location &loc) const
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

bool QueryJob::filterKind(CXCursorKind kind) const
{
    if (mKindFilters.isEmpty())
        return true;
    const String kindSpelling = Symbol::kindSpelling(kind);
    for (auto k : mKindFilters) {
        if (kindSpelling.contains(k, String::CaseInsensitive))
            return true;
    }
    return false;
}
