#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "Message.h"
#include "Path.h"
#include <QtCore>

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
        RdmLog,
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
    QueryMessage(Type type = Invalid, const QByteArray &query = QByteArray(),
                 unsigned flags = 0, QObject *parent = 0);

    QList<QByteArray> pathFilters() const { return mPathFilters; }
    void setPathFilters(const QList<QByteArray> &pathFilters) { mPathFilters = pathFilters; qSort(mPathFilters); }
    int messageId() const { return MessageId; }
    // ### it should be possible to put an already parsed Location in here instead of a query that needs to be reparsed
    QList<QByteArray> query() const { return mQuery; }
    QHash<Path, QByteArray> unsavedFiles() const { return mUnsavedFiles; }
    void setUnsavedFiles(const QHash<Path, QByteArray> &unsavedFiles) { mUnsavedFiles = unsavedFiles; }
    Type type() const { return mType; }
    unsigned flags() const { return mFlags; }
    static unsigned keyFlags(unsigned queryFlags);
    inline unsigned keyFlags() const { return keyFlags(mFlags); }
    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    QList<QByteArray> mQuery;
    Type mType;
    unsigned mFlags;
    QHash<Path, QByteArray> mUnsavedFiles;
    QList<QByteArray> mPathFilters;
};

#endif // QUERYMESSAGE_H
