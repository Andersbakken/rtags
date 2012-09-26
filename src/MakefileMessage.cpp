#include "MakefileMessage.h"
#include <Serializer.h>

MakefileMessage::MakefileMessage(const Path &makefile)
    : mMakefile(makefile), mFlags(0)
{
}

ByteArray MakefileMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mMakefile << mArgs << mExtraCompilerFlags << mFlags;
    }
    return data;
}

void MakefileMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mMakefile >> mArgs >> mExtraCompilerFlags >> mFlags;
}
