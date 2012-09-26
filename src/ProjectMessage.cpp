#include "ProjectMessage.h"
#include <Serializer.h>

ProjectMessage::ProjectMessage(Type type, const Path &path)
    : mType(type), mPath(path), mFlags(0)
{
}

ByteArray ProjectMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << static_cast<int>(mType) << mPath << mArgs << mExtraCompilerFlags << mFlags;
    }
    return data;
}

void ProjectMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    int type;
    stream >> type >> mPath >> mArgs >> mExtraCompilerFlags >> mFlags;
    mType = static_cast<Type>(type);
}
