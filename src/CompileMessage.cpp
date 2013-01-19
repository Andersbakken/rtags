#include "CompileMessage.h"
#include "Serializer.h"

CompileMessage::CompileMessage(const Path &path, const ByteArray &args, const Path &cpp)
    : mPath(path), mArgs(args), mCpp(cpp)
{
}

ByteArray CompileMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mRaw << mPath << mCpp << mArgs;
    }
    return data;
}

void CompileMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mRaw >> mPath >> mCpp >> mArgs;
}
