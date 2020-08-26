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

#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include <stdint.h>
#include <algorithm>
#include <functional>
#include <map>
#include <utility>
#include <vector>

#include "Location.h"
#include "Match.h"
#include "rct/Flags.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Serializer.h"
#include "RTags.h"
#include "RTagsMessage.h"
#include "rct/List.h"
#include "rct/Map.h"
#include "rct/Set.h"
#include "rct/String.h"

struct Symbol;

class QueryMessage : public RTagsMessage
{
public:
    enum { MessageId = QueryId };
    enum Type {
        Invalid,
        GenerateTest,
        AsmFile,
        CheckReindex,
        ClassHierarchy,
        ClearProjects,
        CodeCompleteAt,
        DeadFunctions,
        DebugLocations,
        DeleteProject,
        Dependencies,
        Diagnose,
        DumpCompileCommands,
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
        LastIndexed,
        ListSymbols,
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
        Validate,
        Tokens,
        IncludePath
    };

    enum Flag {
        NoFlag = 0x0,
        NoContext = (1ull << 0),
        FilterSystemIncludes = (1ull << 1),
        StripParentheses = (1ull << 2),
        AllReferences = (1ull << 3),
        ReverseSort = (1ull << 4),
        Elisp = (1ull << 5),
        MatchRegex = (1ull << 6),
        MatchCaseInsensitive = (1ull << 7),
        FindVirtuals = (1ull << 8),
        Silent = (1ull << 9),
        AbsolutePath = (1ull << 10),
        FindFilePreferExact = (1ull << 11),
        SymbolInfoIncludeParents = (1ull << 12),
        SymbolInfoIncludeTargets = (1ull << 13),
        SymbolInfoIncludeReferences = (1ull << 14),
        SymbolInfoIncludeBaseClasses = (1ull << 15),
        DeclarationOnly = (1ull << 16),
        DefinitionOnly = (1ull << 17),
        TargetUsrs = (1ull << 18),
        CursorKind = (1ull << 19),
        DisplayName = (1ull << 20),
        CompilationFlagsOnly = (1ull << 21),
        CompilationFlagsSplitLine = (1ull << 22),
        CompilationFlagsPwd = (1ull << 23),
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
        JSON = (1ull << 42),
        JSONDiagnosticsIncludeSkipped = (1ull << 43),
        CodeCompletionEnabled = (1ull << 44),
        SynchronousDiagnostics = (1ull << 45),
        CodeCompleteNoWait = (1ull << 46),
        SymbolInfoIncludeSourceCode = (1ull << 47),
        AllTargets = (1ull << 48),
        HasMatch = (1ull << 49)
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    struct KindFilters {
        enum Flag {
            None = 0x0,
            InHasWildcards = 0x1,
            InHasCategories = 0x2,
            OutHasWildcards = 0x4,
            OutHasCategories = 0x8
        };
        enum DefinitionType {
            Unset = 0x0,
            NotDefinition = 0x1,
            Definition = 0x2,
            Wildcard = 0x4,
            Category = 0x8
        };
        Flags<Flag> flags;
        Map<String, Flags<DefinitionType> > in, out;
        bool filter(const Symbol &symbol) const;
        void insert(const String &arg);
        bool isEmpty() const { return in.isEmpty() && out.isEmpty(); }
    };

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
    void setPathFilters(const Set<PathFilter> &filters)
    {
        mPathFilters = filters.toList();
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

    void setKindFilters(const KindFilters &kindFilters) { mKindFilters = kindFilters; }
    const KindFilters &kindFilters() const { return mKindFilters; }

    void setUnsavedFiles(const UnsavedFiles &unsavedFiles) { mUnsavedFiles = unsavedFiles; }
    const UnsavedFiles &unsavedFiles() const { return mUnsavedFiles; }

    String query() const { return mQuery; }
    Location location(Location::DecodeFlag flag = Location::NoDecodeFlag) const
    {
        return Location::decode(mQuery, flag);
    }
    void setQuery(String &&query) { mQuery = std::move(query); }
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

    int maxDepth() const { return mMaxDepth; }
    void setMaxDepth(int depth) { mMaxDepth = depth; }

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
        case IsIndexing:
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

    String codeCompletePrefix() const { return mCodeCompletePrefix; }
    void setCodeCompletePrefix(String &&prefix) { mCodeCompletePrefix = std::move(prefix); }
private:
    String mQuery, mCodeCompletePrefix;
    Type mType;
    Flags<QueryMessage::Flag> mFlags;
    int mMax, mMaxDepth, mMinLine, mMaxLine, mBuildIndex;
    List<PathFilter> mPathFilters;
    KindFilters mKindFilters;
    Path mCurrentFile;
    UnsavedFiles mUnsavedFiles;
    int mTerminalWidth;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);
RCT_FLAGS(QueryMessage::Flag);
RCT_FLAGS(QueryMessage::KindFilters::DefinitionType);
RCT_FLAGS(QueryMessage::KindFilters::Flag);

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

inline Serializer &operator<<(Serializer &s, const QueryMessage::KindFilters &filter)
{
    s << filter.flags << filter.in << filter.out;
    return s;
}

inline Deserializer &operator>>(Deserializer &s, QueryMessage::KindFilters &filter)
{
    s >> filter.flags >> filter.in >> filter.out;
    return s;
}

#endif // QUERYMESSAGE_H
