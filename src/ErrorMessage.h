#ifndef ERRORMESSAGE_H
#define ERRORMESSAGE_H

#include "Message.h"
#include <ByteArray.h>
#include <Serializer.h>

class ErrorMessage : public Message
{
public:
    enum { MessageId = 2 };

    enum Error { UnknownError };

    ErrorMessage(Error error = UnknownError);
    ErrorMessage(const ByteArray& message);

    int messageId() const { return MessageId; }

    Error error() const { return mError; }
    ByteArray message() const { return mMessage; }

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    Error mError;
    ByteArray mMessage;
};

DECLARE_NATIVE_TYPE(ErrorMessage::Error);

#endif // ERRORMESSAGE_H
