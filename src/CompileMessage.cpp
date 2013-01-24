#include "CompileMessage.h"
#include "Serializer.h"

CompileMessage::CompileMessage(const Path &path, const ByteArray &args)
    : mPath(path), mArgs(args)
{
}

ByteArray CompileMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mRaw << mPath << mArgs;
    }
    return data;
}

void CompileMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mRaw >> mPath >> mArgs;
}
