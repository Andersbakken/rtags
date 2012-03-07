#include "QueryMessage.h"
#include <QDataStream>

QueryMessage::QueryMessage(QObject* parent)
    : Message(parent)
{
}

QueryMessage::QueryMessage(const QByteArray& query, Type type, QObject* parent)
    : Message(parent), m_type(type)
{
    m_query.append(query);
}

QueryMessage::QueryMessage(const QList<QByteArray>& query, Type type, QObject* parent)
    : Message(parent), m_query(query), m_type(type)
{
}

QByteArray QueryMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << m_query << static_cast<int>(m_type);
    }
    return data;
}

void QueryMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> m_query >> t;
    m_type = static_cast<Type>(t);
}
