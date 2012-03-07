#include "ErrorMessage.h"
#include <QDataStream>

ErrorMessage::ErrorMessage(QObject* parent)
    : Message(parent), m_error(UnknownError)
{
}

ErrorMessage::ErrorMessage(Error error, QObject *parent)
    : Message(parent), m_error(error)
{
    switch (m_error) {
    case UnknownError:
        m_message = "Unknown error";
        break;
    }
}

ErrorMessage::ErrorMessage(const QByteArray& message, QObject* parent)
    : Message(parent), m_error(UnknownError), m_message(message)
{
}

QByteArray ErrorMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(m_error) << m_message;
    }
    return data;
}

void ErrorMessage::fromByteArray(const QByteArray& data)
{
    QDataStream stream(data);
    int err;
    stream >> err >> m_message;
    m_error = static_cast<Error>(err);
}
