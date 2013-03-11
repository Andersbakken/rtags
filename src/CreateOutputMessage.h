#ifndef OutputMessage_h
#define OutputMessage_h

#include "ClientMessage.h"
#include <rct/String.h>

class CreateOutputMessage : public ClientMessage
{
public:
    enum { MessageId = CreateOutputId };

    CreateOutputMessage(int level = 0);

    int level() const;

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);
private:
    int mLevel;
};

#endif
