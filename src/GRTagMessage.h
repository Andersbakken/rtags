#ifndef GRTagMessage_h
#define GRTagMessage_h

#include <List.h>
#include <ByteArray.h>
#include "Message.h"
#include "RTags.h"

class GRTagMessage : public Message
{
public:
    enum { MessageId = 7 };

    GRTagMessage(const Path &path = Path());

    virtual int messageId() const { return MessageId; }

    Path path() const { return mPath; }

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    Path mPath;
};

#endif
