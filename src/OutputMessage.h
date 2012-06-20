#ifndef OUTPUTMESSAGE_H
#define OUTPUTMESSAGE_H

#include "Message.h"
#include <ByteArray.h>

class OutputMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 5 };

    Q_INVOKABLE OutputMessage(QObject *parent);
    OutputMessage(const ByteArray &name, int level = 0, QObject *parent = 0);

    int messageId() const { return MessageId; }

    ByteArray name() const;
    int level() const;

    ByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const ByteArray& data);

private:
    ByteArray mName;
    int mLevel;
};

#endif // OUTPUTMESSAGE_H
