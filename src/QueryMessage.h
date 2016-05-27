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

#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include <algorithm>
#include "Location.h"
#include "Match.h"
#include "rct/Flags.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Serializer.h"
#include "RTags.h"
#include "RTagsMessage.h"

class QueryMessage : public RTagsMessage
{
public:
    enum { MessageId = QueryId };
    enum Type {
        Invalid,
        GenerateTest,
        CheckReindex,
        ClassHierarchy,
        ClearProjects,
        CodeCompleteAt,
        DebugLocations,
        DeleteProject,
        Dependencies,
        Diagnose,
        DumpCompilationDatabase,
        DumpCompletions,
        DumpFile,
        DumpFileMaps,
        FindFile,
        FindSymbols,
        FixIts,
        FollowLocation,
        HasFileManager,
        IncludeFile,
        IsIndexed,
        IsIndexing,
        JobCount,
        ListSymbols,
        PrepareCodeCompleteAt,
        PreprocessFile,
        Project,
        ReferencesLocation,
        ReferencesName,
        Reindex,
        ReloadFileManager,
        RemoveFile,
        SendDiagnostics,
        SetBuffers,
        Sources,
        Status,
        Suspend,
        SymbolInfo,
#ifdef RTAGS_HAS_LUA
        VisitAST,
#endif
        Tokens
    };

    enum Flag {
        NoFlag = 0x0,
        NoContext = (1ull << 0),
        FilterSystemIncludes = (1ull << 1),
        StripParentheses = (1ull << 2),
        AllReferences = (1ull << 3),
        ReverseSort = (1ull << 4),
        Elisp = (1ull << 5),
        IMenu = (1ull << 6),
        MatchRegex = (1ull << 7),
        MatchCaseInsensitive = (1ull << 8),
        FindVirtuals = (1ull << 9),
        Silent = (1ull << 10),
        AbsolutePath = (1ull << 11),
        FindFilePreferExact = (1ull << 12),
        SymbolInfoIncludeParents = (1ull << 13),
        SymbolInfoIncludeTargets = (1ull << 14),
        SymbolInfoIncludeReferences = (1ull << 15),
        SymbolInfoIncludeBaseClasses = (1ull << 16),
        DeclarationOnly = (1ull << 17),
        DefinitionOnly = (1ull << 18),
        AllTargets = (1ull << 19),
        CursorKind = (1ull << 20),
        DisplayName = (1ull << 21),
        CompilationFlagsOnly = (1ull << 22),
        CompilationFlagsSplitLine = (1ull << 23),
        DumpIncludeHeaders = (1ull << 24),
        SilentQuery = (1ull << 25),
        SynchronousCompletions = (1ull << 26),
        NoSortReferencesByInput = (1ull << 27),
        HasLocation = (1ull << 28),
        WildcardSymbolNames = (1ull << 29),
        NoColor = (1ull << 30),
        Rename = (1ull << 31),
        ContainingFunction = (1ull << 32),
        ContainingFunctionLocation = (1ull << 33),
        DumpCheckIncludes = (1ull << 34),
        CurrentProjectOnly = (1ull << 35),
        Wait = (1ull << 36),
        CodeCompleteIncludeMacros = (1ull << 37),
        XML = (1ull << 38),
        NoSpellChecking = (1ull << 39),
        CodeCompleteIncludes = (1ull << 40),
        TokensIncludeSymbols = (1ull << 41),
        JSON = (1ull << 42)
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    struct PathFilter {
        String pattern;
        enum Mode {
            Self,
            Dependency
        } mode;

        bool operator<(const PathFilter &other) const
        {
            const int cmp = pattern.compare(other.pattern);
            if (!cmp)
                return mode < other.mode;
            return cmp < 0;
        }
    };
    const List<PathFilter> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const Set<PathFilter> &pathFilters)
    {
        mPathFilters = pathFilters.toList();
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

#ifdef RTAGS_HAS_LUA
    void setVisitASTScripts(const List<String> &scripts) { mVisitASTScripts = scripts; }
    List<String> visitASTScripts() const { return mVisitASTScripts; }
#endif

    void setKindFilters(const Set<String> &kindFilters) { mKindFilters = kindFilters; }
    const Set<String> &kindFilters() const { return mKindFilters; }

    void setUnsavedFiles(const UnsavedFiles &unsavedFiles) { mUnsavedFiles = unsavedFiles; }
    const UnsavedFiles &unsavedFiles() const { return mUnsavedFiles; }

    String query() const { return mQuery; }
    Location location(Location::DecodeFlag flag = Location::NoDecodeFlag) const
    {
        return Location::decode(mQuery, flag);
    }
    void setQuery(const String &query) { mQuery = query; }
    void setBuildIndex(int index) { mBuildIndex = index; }
    int buildIndex() const { return mBuildIndex; }

    int terminalWidth() const { return mTerminalWidth; }
    void setTerminalWidth(int w) { mTerminalWidth = w; }

    Match match() const;

    void setRangeFilter(int min, int max)
    {
        mMinLine = min;
        mMaxLine = max;
    }

    int minLine() const { return mMinLine; }
    int maxLine() const { return mMaxLine; }

    int max() const { return mMax; }
    void setMax(int max) { mMax = max; }

    Flags<Flag> flags() const { return mFlags; }
    void setFlags(Flags<Flag> flags)
    {
        mFlags = flags;
        switch (mType) {
        case ClearProjects:
        case Project:
        case DeleteProject:
        case Reindex:
        case RemoveFile:
        case Sources:
            mFlags |= MatchRegex;
            break;
        default:
            break;
        }
    }

    void setFlag(Flag flag, bool on = true) { mFlags.set(flag, on); }
    static Flag flagFromString(const String &string);
    static Flags<Location::ToStringFlag> locationToStringFlags(Flags<Flag> queryFlags);
    inline Flags<Location::ToStringFlag> locationToStringFlags() const { return locationToStringFlags(mFlags); }

    virtual void encode(Serializer &serializer) const override;
    virtual void decode(Deserializer &deserializer) override;

    void setCurrentFile(const Path &currentFile) { mCurrentFile = currentFile; }
    Path currentFile() const { return mCurrentFile; }
private:
    String mQuery;
    Type mType;
    Flags<QueryMessage::Flag> mFlags;
    int mMax, mMinLine, mMaxLine, mBuildIndex;
    List<PathFilter> mPathFilters;
    Set<String> mKindFilters;
    Path mCurrentFile;
    UnsavedFiles mUnsavedFiles;
    int mTerminalWidth;
#ifdef RTAGS_HAS_LUA
    List<String> mVisitASTScripts;
#endif
};

inline Serializer &operator<<(Serializer &s, const QueryMessage::PathFilter &filter)
{
    s << filter.pattern << static_cast<uint8_t>(filter.mode);
    return s;
}

inline Deserializer &operator>>(Deserializer &s, QueryMessage::PathFilter &filter)
{
    uint8_t mode;
    s >> filter.pattern >> mode;
    filter.mode = static_cast<QueryMessage::PathFilter::Mode>(mode);
    return s;
}

RCT_FLAGS(QueryMessage::Flag);

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
