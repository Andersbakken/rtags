#include "QueryMessage.h"
#include <QDataStream>

QueryMessage::QueryMessage(QObject* parent)
    : Message(parent), m_type(FollowLocation), m_flags(0)
{
}

QueryMessage::QueryMessage(const QByteArray& query, Type type, int flags,
                           const QHash<Path, QByteArray> &unsavedFiles, QObject* parent)
    : Message(parent), m_type(type), m_flags(flags), m_unsavedFiles(unsavedFiles)
{
    m_query.append(query);
}

QueryMessage::QueryMessage(const QList<QByteArray>& query, Type type, int flags, QObject* parent)
    : Message(parent), m_query(query), m_type(type), m_flags(flags)
{
}

QueryMessage::QueryMessage(const QList<QByteArray>& query, Type type, QObject* parent)
    : Message(parent), m_query(query), m_type(type), m_flags(0)
{
}

QByteArray QueryMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << m_query << static_cast<int>(m_type) << m_flags << m_unsavedFiles;
    }
    return data;
}

void QueryMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> m_query >> t >> m_flags >> m_unsavedFiles;
    m_type = static_cast<Type>(t);
}
