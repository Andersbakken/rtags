#ifndef QUERYMESSAGE_H
#define QUERYMESSAGE_H

#include <QObject>
#include <QList>
#include <QByteArray>
#include "Message.h"

class QueryMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 4 };
    enum Type { FollowLocation, ReferencesLocation, ReferencesName, Recompile, Match, Dump };

    Q_INVOKABLE QueryMessage(QObject* parent = 0);
    QueryMessage(const QByteArray& query, Type type, QObject* parent = 0);
    QueryMessage(const QList<QByteArray>& query, Type type, QObject* parent = 0);

    int messageId() const { return MessageId; }

    QList<QByteArray> query() const { return m_query; }
    Type type() const { return m_type; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    QList<QByteArray> m_query;
    Type m_type;
};

#endif // QUERYMESSAGE_H
