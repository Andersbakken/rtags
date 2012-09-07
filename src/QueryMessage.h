#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "Message.h"
#include "Path.h"
#include <Serializer.h>
#include "Map.h"

class QueryMessage : public Message
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
        RunTest,
        ClearProjects,
        FixIts,
        Errors,
        Reindex,
        Remake,
        Project,
        DeleteProject,
        FindFile,
        Parse,
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
        PathMatchRegExp = 0x100,
        AbsolutePath = 0x200,
        DisableGRTags = 0x400
    };

    typedef Map<Path, ByteArray> UnsavedFilesMap;
    QueryMessage(Type type = Invalid, const ByteArray &query = ByteArray(),
                 unsigned flags = 0, const UnsavedFilesMap &unsaved = UnsavedFilesMap());

    const List<ByteArray> &pathFilters() const { return mPathFilters; }
    void setPathFilters(const List<ByteArray> &pathFilters)
    {
        mPathFilters = pathFilters;
        std::sort(mPathFilters.begin(), mPathFilters.end());
    }

    int messageId() const { return MessageId; }
    // ### it should be possible to put an already parsed Location in here instead of a query that needs to be reparsed
    ByteArray query() const { return mQuery; }

    Map<Path, ByteArray> unsavedFiles() const { return mUnsavedFiles; }

    Type type() const { return mType; }
    unsigned flags() const { return mFlags; }
    static unsigned keyFlags(unsigned queryFlags);
    inline unsigned keyFlags() const { return keyFlags(mFlags); }

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    ByteArray mQuery;
    Type mType;
    unsigned mFlags;
    List<ByteArray> mPathFilters;
    Map<Path, ByteArray> mUnsavedFiles;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
