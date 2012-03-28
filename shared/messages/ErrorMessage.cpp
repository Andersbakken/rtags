#include "ErrorMessage.h"
#include <QDataStream>

ErrorMessage::ErrorMessage(QObject* parent)
    : Message(parent), mError(UnknownError)
{
}

ErrorMessage::ErrorMessage(Error error, QObject *parent)
    : Message(parent), mError(error)
{
    switch (mError) {
    case UnknownError:
        mMessage = "Unknown error";
        break;
    }
}

ErrorMessage::ErrorMessage(const QByteArray& message, QObject* parent)
    : Message(parent), mError(UnknownError), mMessage(message)
{
}

QByteArray ErrorMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(mError) << mMessage;
    }
    return data;
}

void ErrorMessage::fromByteArray(const QByteArray& data)
{
    QDataStream stream(data);
    int err;
    stream >> err >> mMessage;
    mError = static_cast<Error>(err);
}
