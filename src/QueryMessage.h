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

#include "RTagsMessage.h"
#include "RTags.h"
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <rct/Hash.h>
#include "Match.h"
#include "Location.h"
#include <rct/Flags.h>

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
        DeleteProject,
        Dependencies,
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
        SymbolInfo
    };

    enum Flag {
        NoFlag = 0x000000000,
        NoContext = 0x000000001,
        FilterSystemIncludes = 0x000000004,
        StripParentheses = 0x000000008,
        AllReferences = 0x000000010,
        ReverseSort = 0x000000020,
        ElispList = 0x000000040,
        IMenu = 0x000000080,
        MatchRegex = 0x000000100,
        MatchCaseInsensitive = 0x000000200,
        FindVirtuals = 0x000000400,
        Silent = 0x000000800,
        AbsolutePath = 0x000001000,
        FindFilePreferExact = 0x000002000,
        SymbolInfoIncludeParents = 0x000004000,
        SymbolInfoExcludeTargets = 0x000008000,
        SymbolInfoExcludeReferences = 0x000010000,
        DeclarationOnly = 0x000020000,
        DefinitionOnly = 0x000040000,
        AllTargets = 0x000080000,
        CursorKind = 0x000100000,
        DisplayName = 0x000200000,
        CompilationFlagsOnly = 0x000400000,
        CompilationFlagsSplitLine = 0x000800000,
        DumpIncludeHeaders = 0x001000000,
        SilentQuery = 0x002000000,
        SynchronousCompletions = 0x004000000,
        NoSortReferencesByInput = 0x008000000,
        HasLocation = 0x010000000,
        WildcardSymbolNames = 0x020000000,
        NoColor = 0x040000000,
        Rename = 0x080000000,
        ContainingFunction = 0x100000000,
        Wait = 0x200000000
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    const List<String> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const Set<String> &pathFilters)
    {
        mPathFilters = pathFilters.toList();
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

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
    static Flags<Location::KeyFlag> keyFlags(Flags<Flag> queryFlags);
    inline Flags<Location::KeyFlag> keyFlags() const { return keyFlags(mFlags); }

    virtual void encode(Serializer &serializer) const override;
    virtual void decode(Deserializer &deserializer) override;

    void setCurrentFile(const Path &currentFile) { mCurrentFile = currentFile; }
    Path currentFile() const { return mCurrentFile; }
private:
    String mQuery;
    Type mType;
    Flags<QueryMessage::Flag> mFlags;
    int mMax, mMinLine, mMaxLine, mBuildIndex;
    List<String> mPathFilters;
    Set<String> mKindFilters;
    Path mCurrentFile;
    UnsavedFiles mUnsavedFiles;
    int mTerminalWidth;
};

RCT_FLAGS(QueryMessage::Flag);

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
