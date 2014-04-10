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
        LoadCompilationDatabase,
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
        SuspendFile,
        SyncProject,
        UnloadProject
    };

    enum Flag {
        NoContext = 0x0000001,
        FilterSystemIncludes = 0x0000004,
        StripParentheses = 0x0000008,
        AllReferences = 0x0000010,
        ReverseSort = 0x0000020,
        ElispList = 0x0000040,
        IMenu = 0x0000080,
        MatchRegexp = 0x0000100,
        MatchCaseInsensitive = 0x0000200,
        FindVirtuals = 0x0000400,
        Silent = 0x0000800,
        AbsolutePath = 0x0001000,
        FindFilePreferExact = 0x0002000,
        CursorInfoIncludeParents = 0x0004000,
        CursorInfoIncludeTargets = 0x0008000,
        CursorInfoIncludeReferences = 0x0010000,
        DeclarationOnly = 0x0020000,
        ContainingFunction = 0x0040000,
        WaitForLoadProject = 0x0080000,
        CursorKind = 0x0100000,
        DisplayName = 0x0200000,
        CompilationFlagsOnly = 0x0400000,
        CompilationFlagsSplitLine = 0x0800000,
        DumpIncludeHeaders = 0x1000000,
        SilentQuery = 0x2000000,
        SynchronousCompletions = 0x4000000,
        NoSortReferencesByInput = 0x8000000
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    const List<String> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const List<String> &pathFilters)
    {
        mPathFilters = pathFilters;
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

    void setUnsavedFiles(const Hash<Path, String> &unsavedFiles) { mUnsavedFiles = unsavedFiles; }
    const Hash<Path, String> &unsavedFiles() const { return mUnsavedFiles; }

    void setContext(const String &context) { mContext = context; }
    String context() const { return mContext; }

    String query() const { return mQuery; }
    Location location() const { return Location::decodeClientLocation(mQuery); }
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

    void addProject(const Path &project) { mProjects.append(project); }
    void setProjects(const List<String> &projects) { mProjects = projects; }
    List<String> projects() const { return mProjects; }
private:
    String mQuery, mContext;
    Type mType;
    unsigned mFlags;
    int mMax, mMinLine, mMaxLine, mBuildIndex;
    List<String> mPathFilters;
    List<String> mProjects;
    Hash<Path, String> mUnsavedFiles;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
