#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "ClientMessage.h"
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <rct/Map.h>
#include "Match.h"
#include "Location.h"

class QueryMessage : public ClientMessage
{
public:
    enum { MessageId = QueryId };
    enum Type {
        Builds,
        CodeCompletionEnabled,
        ClearProjects,
        CursorInfo,
        DeleteProject,
        Dependencies,
        DumpFile,
        FindFile,
        FindSymbols,
        FixIts,
        FollowLocation,
        HasFileManager,
        Invalid,
        IsIndexed,
        IsIndexing,
        JSON,
        JobCount,
        ListSymbols,
        LoadCompilationDatabase,
        PreprocessFile,
        Project,
        ReferencesLocation,
        ReferencesName,
        Reindex,
        ReloadFileManager,
        ReloadProjects,
        RemoveFile,
        Shutdown,
        Status,
        UnloadProject,
        SuspendFile
    };

    enum Flag {
        NoContext = 0x00001,
        LineNumbers = 0x00002,
        FilterSystemIncludes = 0x00004,
        StripParentheses = 0x00008,
        AllReferences = 0x00010,
        ReverseSort = 0x00020,
        ElispList = 0x00040,
        IMenu = 0x00080,
        MatchRegexp = 0x00100,
        MatchCaseInsensitive = 0x00200,
        FindVirtuals = 0x00400,
        Silent = 0x00800,
        AbsolutePath = 0x01000,
        FindFilePreferExact = 0x02000,
        CursorInfoIncludeParents = 0x04000,
        CursorInfoIncludeTargets = 0x08000,
        CursorInfoIncludeReferences = 0x10000,
        DeclarationOnly = 0x20000,
        ContainingFunction = 0x40000,
        WaitForLoadProject = 0x80000
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    const List<String> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const List<String> &pathFilters)
    {
        mPathFilters = pathFilters;
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

    void setContext(const String &context) { mContext = context; }
    String context() const { return mContext; }

    String query() const { return mQuery; }
    Location location() const { return Location::decodeClientLocation(mQuery); }
    void setQuery(const String &query) { mQuery = query; }

    Match match() const;

    void setRangeFilter(int min, int max)
    {
        mMinOffset = min;
        mMaxOffset = max;
    }

    int minOffset() const { return mMinOffset; }
    int maxOffset() const { return mMaxOffset; }

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
        case Builds:
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

    void setProjects(const List<String> &projects) { mProjects = projects; }
    List<String> projects() const { return mProjects; }
private:
    String mQuery, mContext;
    Type mType;
    unsigned mFlags;
    int mMax, mMinOffset, mMaxOffset;
    List<String> mPathFilters;
    List<String> mProjects;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
