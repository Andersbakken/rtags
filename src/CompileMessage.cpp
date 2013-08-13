#include "CompileMessage.h"
#include <rct/Serializer.h>

CompileMessage::CompileMessage(const Path &cwd, const String &args)
    : ClientMessage(MessageId), mWorkingDirectory(cwd), mArgs(args)
{
}

void CompileMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mWorkingDirectory << mArgs << mProjects;
}

void CompileMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mWorkingDirectory >> mArgs >> mProjects;
}
