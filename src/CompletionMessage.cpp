#include "CompletionMessage.h"
#include "RTags.h"
#include <rct/Serializer.h>


CompletionMessage::CompletionMessage(unsigned flags, const Path &path, int line, int column, int pos)
    : ClientMessage(MessageId), mFlags(flags), mPath(path), mLine(line), mColumn(column), mPos(pos)
{
}

void CompletionMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mFlags << mPath << mLine << mColumn << mPos << mContents << mProjects;
}

void CompletionMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mFlags >> mPath >> mLine >> mColumn >> mPos >> mContents >> mProjects;
}
