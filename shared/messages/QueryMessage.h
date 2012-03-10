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
        Match,
        Dump,
        CodeComplete,
        CursorInfo
    };

    Q_INVOKABLE QueryMessage(QObject* parent = 0);
    QueryMessage(const QByteArray& query, Type type,
                 const QHash<Path, QByteArray> &unsavedFiles, QObject* parent = 0);
    QueryMessage(const QList<QByteArray>& query, Type type, QObject* parent = 0);

    int messageId() const { return MessageId; }

    QList<QByteArray> query() const { return m_query; }
    Type type() const { return m_type; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    QList<QByteArray> m_query;
    Type m_type;
    QHash<Path, QByteArray> m_unsavedFiles;
};

#endif // QUERYMESSAGE_H
