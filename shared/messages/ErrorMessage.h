#ifndef ERRORMESSAGE_H
#define ERRORMESSAGE_H

#include <QObject>
#include "Message.h"
#include <ByteArray.h>

class ErrorMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 2 };

    enum Error { UnknownError };

    Q_INVOKABLE ErrorMessage(QObject* parent = 0);
    ErrorMessage(Error error, QObject* parent = 0);
    ErrorMessage(const ByteArray& message, QObject* parent = 0);

    int messageId() const { return MessageId; }

    Error error() const { return mError; }
    ByteArray message() const { return mMessage; }

    ByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const ByteArray& data);

private:
    Error mError;
    ByteArray mMessage;
};

#endif // ERRORMESSAGE_H
