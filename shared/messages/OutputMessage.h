#ifndef OUTPUTMESSAGE_H
#define OUTPUTMESSAGE_H

#include "Message.h"

class OutputMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 5 };

    Q_INVOKABLE OutputMessage(QObject *parent);
    OutputMessage(const QByteArray &name, int level = 0, QObject *parent = 0);

    int messageId() const { return MessageId; }

    QByteArray name() const;
    int level() const;

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    QByteArray mName;
    int mLevel;
};

#endif // OUTPUTMESSAGE_H
