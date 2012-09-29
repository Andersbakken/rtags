#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "ClientMessage.h"
#include "Path.h"
#include <Serializer.h>
#include "Map.h"
#include "Location.h"

class QueryMessage : public ClientMessage
{
public:
    enum { MessageId = 4 };
    enum Type {
        Invalid,
        FollowLocation,
        ReferencesLocation,
        ReferencesName,
        ListSymbols,
        FindSymbols,
        Status,
        Test,
        CursorInfo,
        ClearProjects,
        FixIts,
        Errors,
        Reindex,
        Project,
        DeleteProject,
        FindFile,
        DumpFile,
        Shutdown
    };

    enum Flag {
        NoContext = 0x001,
        LineNumbers = 0x002,
        FilterSystemIncludes = 0x004,
        SkipParentheses = 0x008,
        ReferencesForRenameSymbol = 0x010,
        ReverseSort = 0x020,
        ElispList = 0x040,
        WaitForIndexing = 0x080,
        MatchRegexp = 0x100,
        AbsolutePath = 0x200,
        FindVirtuals = 0x400
    };

    typedef Map<Path, ByteArray> UnsavedFilesMap;
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

    Map<Path, ByteArray> unsavedFiles() const { return mUnsavedFiles; }
    void setUnsavedFiles(const Map<Path, ByteArray> &unsavedFiles) { mUnsavedFiles = unsavedFiles; }

    int max() const { return mMax; }
    void setMax(int max) { mMax = max; }

    int lockTimeout() const { return mLockTimeout; }
    void setLockTimeout(int timeout) { mLockTimeout = timeout; }


    unsigned flags() const { return mFlags; }
    void setFlags(unsigned flags) { mFlags = flags; }

    static unsigned keyFlags(unsigned queryFlags);
    inline unsigned keyFlags() const { return keyFlags(mFlags); }

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    ByteArray mQuery;
    Type mType;
    unsigned mFlags;
    int mMax, mLockTimeout;
    List<ByteArray> mPathFilters;
    Map<Path, ByteArray> mUnsavedFiles;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
