#include "CreateOutputMessage.h"
#include <rct/Serializer.h>

CreateOutputMessage::CreateOutputMessage(int level)
    : ClientMessage(MessageId), mLevel(level)
{
}

int CreateOutputMessage::level() const
{
    return mLevel;
}

void CreateOutputMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mLevel;
}

void CreateOutputMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mLevel;
}
