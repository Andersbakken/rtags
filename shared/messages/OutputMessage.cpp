#include "OutputMessage.h"
#include <Serializer.h>

OutputMessage::OutputMessage(QObject *parent)
    : mLevel(0)
{
}

OutputMessage::OutputMessage(const ByteArray &name, int level, QObject *parent)
    : mName(name), mLevel(level)
{
}

ByteArray OutputMessage::name() const
{
    return mName;
}

int OutputMessage::level() const
{
    return mLevel;
}

ByteArray OutputMessage::toByteArray() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mName << mLevel;
    }
    return data;
}

void OutputMessage::fromByteArray(const ByteArray &data)
{
    Deserializer ds(data.constData(), data.size());
    ds >> mName >> mLevel;
}
