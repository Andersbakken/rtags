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

ErrorMessage::ErrorMessage(const ByteArray& message, QObject* parent)
    : Message(parent), mError(UnknownError), mMessage(message)
{
}

ByteArray ErrorMessage::toByteArray() const
{
    QByteArray data;
    {
        QDataStream stream(&data, QIODevice::WriteOnly);
        stream << static_cast<int>(mError) << mMessage;
    }
    return ByteArray(data.constData(), data.size());
}

void ErrorMessage::fromByteArray(const ByteArray &data)
{
    const QByteArray ba = QByteArray::fromRawData(data.constData(), data.size());
    QDataStream stream(ba);
    int err;
    stream >> err >> mMessage;
    mError = static_cast<Error>(err);
}
