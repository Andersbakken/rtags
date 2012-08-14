#include "GRTagMessage.h"
#include <Serializer.h>

GRTagMessage::GRTagMessage(const Path &dir)
    : mPath(dir)
{
}

ByteArray GRTagMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mPath;
    }
    return data;
}

void GRTagMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mPath;
}
