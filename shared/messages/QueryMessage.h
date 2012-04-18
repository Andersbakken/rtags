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
        FollowLocation,
        ReferencesLocation,
        ReferencesName,
        Recompile,
        ListSymbols,
        FindSymbols,
        Dump,
        Status,
        Poll
    };

    enum Flag {
        NoContext = 0x01,
        LineNumbers = 0x02
    };

    Q_INVOKABLE QueryMessage(QObject* parent = 0);
    QueryMessage(Type type, const QList<QByteArray> &msg);
    QueryMessage(Type type, const QByteArray &query, unsigned flags,
                 const QHash<Path, QByteArray> &unsavedFiles,
                 const QSet<QByteArray> &pathFilters,
                 QObject *parent = 0);

    QSet<QByteArray> pathFilters() const { return mPathFilters; }
    int messageId() const { return MessageId; }
    QList<QByteArray> query() const { return mQuery; }
    QHash<Path, QByteArray> unsavedFiles() const { return mUnsavedFiles; }
    Type type() const { return mType; }
    unsigned flags() const { return mFlags; }
    unsigned keyFlags() const;
    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    QList<QByteArray> mQuery;
    Type mType;
    unsigned mFlags;
    QHash<Path, QByteArray> mUnsavedFiles;
    QSet<QByteArray> mPathFilters;
};

#endif // QUERYMESSAGE_H
