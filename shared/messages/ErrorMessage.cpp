#include "ErrorMessage.h"
#include <Serializer.h>

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
    ByteArray data;
    {
        Serializer s(data);
        s << mError << mMessage;
    }
    return data;
}

void ErrorMessage::fromByteArray(const ByteArray &data)
{
    Deserializer ds(data.constData(), data.size());
    ds >> mError >> mMessage;
}
