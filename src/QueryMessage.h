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

#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "RTagsMessage.h"
#include "RTags.h"
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <rct/Hash.h>
#include "Match.h"
#include "Location.h"

class QueryMessage : public RTagsMessage
{
public:
    enum { MessageId = QueryId };
    enum Type {
        CheckReindex,
        ClearProjects,
        CodeCompleteAt,
        CursorInfo,
        DeleteProject,
        Dependencies,
        DumpCompletions,
        DumpFile,
        FindFile,
        FindSymbols,
        FixIts,
        FollowLocation,
        HasFileManager,
        Invalid,
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
        ReloadProjects,
        RemoveFile,
        SendDiagnostics,
        Shutdown,
        Sources,
        Status,
        Suspend,
        SyncProject,
        UnloadProject
    };

    enum Flag {
        NoContext = 0x00000001,
        FilterSystemIncludes = 0x00000004,
        StripParentheses = 0x00000008,
        AllReferences = 0x00000010,
        ReverseSort = 0x00000020,
        ElispList = 0x00000040,
        IMenu = 0x00000080,
        MatchRegexp = 0x00000100,
        MatchCaseInsensitive = 0x00000200,
        FindVirtuals = 0x00000400,
        Silent = 0x00000800,
        AbsolutePath = 0x00001000,
        FindFilePreferExact = 0x00002000,
        CursorInfoIncludeParents = 0x00004000,
        CursorInfoIncludeTargets = 0x00008000,
        CursorInfoIncludeReferences = 0x00010000,
        DeclarationOnly = 0x00020000,
        ContainingFunction = 0x00040000,
        WaitForLoadProject = 0x00080000,
        CursorKind = 0x00100000,
        DisplayName = 0x00200000,
        CompilationFlagsOnly = 0x00400000,
        CompilationFlagsSplitLine = 0x00800000,
        DumpIncludeHeaders = 0x01000000,
        SilentQuery = 0x02000000,
        SynchronousCompletions = 0x04000000,
        NoSortReferencesByInput = 0x08000000,
        HasLocation = 0x10000000,
        WildcardSymbolNames = 0x20000000
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    const List<String> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const List<String> &pathFilters)
    {
        mPathFilters = pathFilters;
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

    void setUnsavedFiles(const UnsavedFiles &unsavedFiles) { mUnsavedFiles = unsavedFiles; }
    const UnsavedFiles &unsavedFiles() const { return mUnsavedFiles; }

    String query() const { return mQuery; }
    Location location() const { return Location::decode(mQuery); }
    void setQuery(const String &query) { mQuery = query; }
    void setBuildIndex(int index) { mBuildIndex = index; }
    int buildIndex() const { return mBuildIndex; }

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

    unsigned flags() const { return mFlags; }
    void setFlags(unsigned flags)
    {
        mFlags = flags;
        switch (mType) {
        case ClearProjects:
        case Project:
        case DeleteProject:
        case Reindex:
        case RemoveFile:
        case UnloadProject:
        case Sources:
            mFlags |= MatchRegexp;
            break;
        default:
            break;
        }
    }

    static unsigned keyFlags(unsigned queryFlags);
    inline unsigned keyFlags() const { return keyFlags(mFlags); }

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);

    void addProject(const Path &project) { mCurrentFile.append(project); }
    void setCurrentFile(const Path &currentFile) { mCurrentFile = currentFile; }
    Path currentFile() const { return mCurrentFile; }
private:
    String mQuery;
    Type mType;
    unsigned mFlags;
    int mMax, mMinLine, mMaxLine, mBuildIndex;
    List<String> mPathFilters;
    Path mCurrentFile;
    UnsavedFiles mUnsavedFiles;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
