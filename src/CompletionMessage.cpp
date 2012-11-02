#include "CompletionMessage.h"
#include "RTags.h"
#include "Serializer.h"


CompletionMessage::CompletionMessage(const Path &path, int line, int column)
    : mPath(path), mLine(line), mColumn(column)
{
}

ByteArray CompletionMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mRaw << mPath << mLine << mColumn << mContents << mProjects;
    }
    return data;
}

void CompletionMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mRaw >> mPath >> mLine >> mColumn >> mContents >> mProjects;
}
