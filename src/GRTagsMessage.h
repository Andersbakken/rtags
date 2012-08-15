#ifndef GRTagsMessage_h
#define GRTagsMessage_h

#include <List.h>
#include <ByteArray.h>
#include "Message.h"
#include "RTags.h"

class GRTagsMessage : public Message
{
public:
    enum { MessageId = 7 };

    GRTagsMessage(const Path &path = Path());

    virtual int messageId() const { return MessageId; }

    Path path() const { return mPath; }

    ByteArray encode() const;
    void fromData(const char *data, int size);

private:
    Path mPath;
};

#endif
