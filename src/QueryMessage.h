#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "Message.h"
#include "Path.h"
#include <QtCore>
#include <Serializer.h>

class QueryMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 4 };
    enum Type {
        Invalid,
        FollowLocation,
        ReferencesLocation,
        ReferencesName,
        ListSymbols,
        FindSymbols,
        Dump,
        Status,
        Test,
        CursorInfo,
        RunTest,
        ClearDatabase,
        FixIts,
        Errors,
        Reindex,
        Remake,
        Shutdown
    };

    enum Flag {
        NoContext = 0x01,
        LineNumbers = 0x02,
        FilterSystemIncludes = 0x04,
        SkipParentheses = 0x08,
        IncludeDeclarationsAndDefinitions = 0x10,
        ReverseSort = 0x20,
        ElispList = 0x40,
        SameFile = 0x80
    };

    Q_INVOKABLE QueryMessage(QObject *parent);
    QueryMessage(Type type = Invalid, const ByteArray &query = ByteArray(),
                 unsigned flags = 0, QObject *parent = 0);

    List<ByteArray> pathFilters() const { return mPathFilters; }
    void setPathFilters(const List<ByteArray> &pathFilters) { mPathFilters = pathFilters; qSort(mPathFilters); }

    int messageId() const { return MessageId; }
    // ### it should be possible to put an already parsed Location in here instead of a query that needs to be reparsed
    List<ByteArray> query() const { return mQuery; }

    Type type() const { return mType; }
    unsigned flags() const { return mFlags; }
    static unsigned keyFlags(unsigned queryFlags);
    inline unsigned keyFlags() const { return keyFlags(mFlags); }

    ByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const ByteArray& data);

private:
    List<ByteArray> mQuery;
    Type mType;
    unsigned mFlags;
    List<ByteArray> mPathFilters;
};

DECLARE_NATIVE_TYPE(QueryMessage::Type);

#endif // QUERYMESSAGE_H
