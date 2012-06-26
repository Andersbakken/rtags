#include "MakefileMessage.h"
#include <Serializer.h>

MakefileMessage::MakefileMessage(const Path &makefile, const List<ByteArray> &args, const List<ByteArray> &extraFlags)
    : mMakefile(makefile), mArgs(args), mExtraFlags(extraFlags)
{
}

ByteArray MakefileMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mMakefile << mArgs << mExtraFlags;
    }
    return data;
}

void MakefileMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mMakefile >> mArgs >> mExtraFlags;
}
