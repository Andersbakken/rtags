#include "CreateOutputMessage.h"
#include <rct/Serializer.h>

CreateOutputMessage::CreateOutputMessage(int level)
    : mLevel(level)
{
}

int CreateOutputMessage::level() const
{
    return mLevel;
}

String CreateOutputMessage::encode() const
{
    String data;
    {
        Serializer stream(data);
        stream << mRaw << mLevel;
    }
    return data;
}

void CreateOutputMessage::fromData(const char *data, int size)
{
    Deserializer ds(data, size);
    ds >> mRaw >> mLevel;
}
