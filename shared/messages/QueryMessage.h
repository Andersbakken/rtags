#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include "Message.h"
#include "Path.h"
#include <QByteArray>
#include <QHash>
#include <QList>
#include <QObject>

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
        Poke
    };
    enum Flag {
        SkipParen = 0x01,
        NoContext = 0x02
    };

    Q_INVOKABLE QueryMessage(QObject* parent = 0);
    QueryMessage(const QByteArray& query, Type type, int flags,
                 const QHash<Path, QByteArray> &unsavedFiles, QObject* parent = 0);
    QueryMessage(const QList<QByteArray>& query, Type type, int flags, QObject* parent = 0);
    QueryMessage(const QList<QByteArray>& query, Type type, QObject* parent = 0);

    int messageId() const { return MessageId; }

    QList<QByteArray> query() const { return mQuery; }

    QHash<Path, QByteArray> unsavedFiles() const { return mUnsavedFiles; }
    Type type() const { return mType; }
    int flags() const { return mFlags; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    QList<QByteArray> mQuery;
    Type mType;
    int mFlags;
    QHash<Path, QByteArray> mUnsavedFiles;
};

#endif // QUERYMESSAGE_H
