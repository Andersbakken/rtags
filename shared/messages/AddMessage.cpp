#include "AddMessage.h"
#include <QDataStream>

AddMessage::AddMessage(QObject* parent)
    : Message(parent)
{
}

AddMessage::AddMessage(RTags::UnitType type, const QByteArray& input, const QByteArray& output,
                       const QList<QByteArray>& args, const QList<QByteArray>& pchs,
                       QObject* parent)
    : Message(parent), m_type(type), m_input(input), m_output(output), m_args(args), m_pchs(pchs)
{
}

QByteArray AddMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(m_type) << m_input << m_output << m_args << m_pchs;
    }
    return data;
}

void AddMessage::fromByteArray(const QByteArray& data)
{
    int t;
    QDataStream stream(data);
    stream >> t >> m_input >> m_output >> m_args >> m_pchs;
    m_type = static_cast<RTags::UnitType>(t);
}
