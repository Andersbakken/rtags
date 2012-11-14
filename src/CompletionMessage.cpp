#include "CompletionMessage.h"
#include "RTags.h"
#include "Serializer.h"


CompletionMessage::CompletionMessage(unsigned flags, const Path &path, int line, int column, int pos)
    : mFlags(flags), mPath(path), mLine(line), mColumn(column), mPos(pos)
{
}

ByteArray CompletionMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mRaw << mFlags << mPath << mLine << mColumn << mPos << mContents << mProjects;
    }
    return data;
}

void CompletionMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mRaw >> mFlags >> mPath >> mLine >> mColumn >> mPos >> mContents >> mProjects;
}
