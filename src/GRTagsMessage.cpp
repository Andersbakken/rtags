#include "GRTagsMessage.h"
#include <Serializer.h>

GRTagsMessage::GRTagsMessage(const Path &dir)
    : mPath(dir)
{
}

ByteArray GRTagsMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mPath;
    }
    return data;
}

void GRTagsMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mPath;
}
