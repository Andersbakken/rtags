#include "MakefileMessage.h"
#include <Serializer.h>

MakefileMessage::MakefileMessage(QObject *parent)
    : Message(parent)
{
}

MakefileMessage::MakefileMessage(const Path &makefile, const List<ByteArray> &args,
                                 const List<ByteArray> &extraFlags, QObject *parent)
    : Message(parent), mMakefile(makefile), mArgs(args), mExtraFlags(extraFlags)
{
}

ByteArray MakefileMessage::toByteArray() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mMakefile; // << mArgs << mExtraFlags;
    }
    return data;
}

void MakefileMessage::fromByteArray(const ByteArray &data)
{
    Deserializer stream(data.constData(), data.size());
    stream >> mMakefile; // >> mArgs >> mExtraFlags;
}
