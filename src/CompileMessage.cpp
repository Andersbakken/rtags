#include "CompileMessage.h"
#include <rct/Serializer.h>

CompileMessage::CompileMessage(const Path &path, const String &args)
    : ClientMessage(MessageId), mPath(path), mArgs(args)
{
}

void CompileMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mPath << mArgs;
}

void CompileMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mPath >> mArgs;
}
