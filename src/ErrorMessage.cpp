#include "ErrorMessage.h"
#include <Serializer.h>

ErrorMessage::ErrorMessage(Error error)
    : mError(error)
{
    switch (mError) {
    case UnknownError:
        mMessage = "Unknown error";
        break;
    }
}

ErrorMessage::ErrorMessage(const ByteArray& message)
    : mError(UnknownError), mMessage(message)
{
}

ByteArray ErrorMessage::encode() const
{
    ByteArray data;
    {
        Serializer s(data);
        s << mError << mMessage;
    }
    return data;
}

void ErrorMessage::fromData(const char *data, int size)
{
    Deserializer ds(data, size);
    ds >> mError >> mMessage;
}
