#include "CreateOutputMessage.h"
#include <Serializer.h>

CreateOutputMessage::CreateOutputMessage(int level)
    : mLevel(level)
{
}

int CreateOutputMessage::level() const
{
    return mLevel;
}

ByteArray CreateOutputMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mLevel;
    }
    return data;
}

void CreateOutputMessage::fromData(const char *data, int size)
{
    Deserializer ds(data, size);
    ds >> mLevel;
}
