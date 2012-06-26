#ifndef OutputMessage_h
#define OutputMessage_h

#include "Message.h"
#include <ByteArray.h>

class CreateOutputMessage : public Message
{
public:
    enum { MessageId = 5 };

    CreateOutputMessage(int level = 0);

    int messageId() const { return MessageId; }

    int level() const;

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    int mLevel;
};

#endif
