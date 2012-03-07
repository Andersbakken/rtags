#include "AddMessage.h"
#include <QDataStream>

AddMessage::AddMessage(QObject* parent)
    : Message(parent)
{
}

AddMessage::AddMessage(Type type, const QByteArray& input, const QByteArray& output,
                       const QList<QByteArray>& args, QObject* parent)
    : Message(parent), m_type(type), m_input(input), m_output(output), m_args(args)
{
}

QByteArray AddMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(m_type) << m_input << m_output << m_args;
    }
    return data;
}

void AddMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> t >> m_input >> m_output >> m_args;
    m_type = static_cast<Type>(t);
}
