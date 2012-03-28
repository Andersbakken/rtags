#ifndef ERRORMESSAGE_H
#define ERRORMESSAGE_H

#include <QObject>
#include "Message.h"

class ErrorMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 2 };

    enum Error { UnknownError };

    Q_INVOKABLE ErrorMessage(QObject* parent = 0);
    ErrorMessage(Error error, QObject* parent = 0);
    ErrorMessage(const QByteArray& message, QObject* parent = 0);

    int messageId() const { return MessageId; }

    Error error() const { return mError; }
    QByteArray message() const { return mMessage; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    Error mError;
    QByteArray mMessage;
};

#endif // ERRORMESSAGE_H
