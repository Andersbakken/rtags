#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "ClientMessage.h"
#include "Path.h"
#include "Serializer.h"
#include "Map.h"
#include "Match.h"
#include "Location.h"

class QueryMessage : public ClientMessage
{
public:
    enum { MessageId = QueryId };
    enum Type {
        ClearProjects,
        CursorInfo,
        DeleteProject,
        DumpFile,
        FindFile,
        FindSymbols,
        FixIts,
        FollowLocation,
        HasFileManager,
        Invalid,
        IsIndexed,
        ListSymbols,
        PreprocessFile,
        Project,
        ReferencesLocation,
        ReferencesName,
        Reindex,
        ReloadProjects,
        Shutdown,
        Status,
        UnloadProject
    };

    enum Flag {
        NoContext = 0x0001,
        LineNumbers = 0x0002,
        FilterSystemIncludes = 0x0004,
        SkipParentheses = 0x0008,
        ReferencesForRenameSymbol = 0x0010,
        ReverseSort = 0x0020,
        ElispList = 0x0040,
        WaitForIndexing = 0x0080,
        MatchRegexp = 0x0100,
        MatchCaseInsensitive = 0x0200,
        FindVirtuals = 0x0400,
        Silent = 0x0800,
        AbsolutePath = 0x1000,
        FindFilePreferExact = 0x2000
    };

    QueryMessage(Type type = Invalid);

    Type type() const { return mType; }

    const List<ByteArray> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const List<ByteArray> &pathFilters)
    {
        mPathFilters = pathFilters;
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

    int messageId() const { return MessageId; }

    ByteArray query() const { return mQuery; }
    Location location() const { return Location::decodeClientLocation(mQuery); }
    void setQuery(const ByteArray &query) { mQuery = query; }

    Match match() const;

    int max() const { return mMax; }
    void setMax(int max) { mMax = max; }

    unsigned flags() const { return mFlags; }
    void setFlags(unsigned flags) { mFlags = flags; }

    static unsigned keyFlags(unsigned queryFlags);
    inline unsigned keyFlags() const { return keyFlags(mFlags); }

    ByteArray encode() const;
    void fromData(const char *data, int size);

    void setProjects(const List<ByteArray> &projects) { mProjects = projects; }
    List<ByteArray> projects() const { return mProjects; }
private:
    ByteArray mQuery;
    Type mType;
    unsigned mFlags;
    int mMax;
    List<ByteArray> mPathFilters;
    List<ByteArray> mProjects;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
