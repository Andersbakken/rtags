#include "ProjectMessage.h"
#include "Serializer.h"

ProjectMessage::ProjectMessage(const Path &path, const ByteArray &args)
    : mPath(path), mArgs(args)
{
}

ByteArray ProjectMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mRaw << mPath << mArgs;
    }
    return data;
}

void ProjectMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mRaw >> mPath >> mArgs;
}
