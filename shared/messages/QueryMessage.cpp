#include "QueryMessage.h"
#include "RTags.h"
#include <Serializer.h>

QueryMessage::QueryMessage(QObject *parent)
    : Message(parent), mType(Invalid), mFlags(0)
{
}

QueryMessage::QueryMessage(Type type, const ByteArray& query, unsigned flags, QObject* parent)
    : Message(parent), mType(type), mFlags(flags)
{
    mQuery.append(query);
}

ByteArray QueryMessage::toByteArray() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mQuery << mType << mFlags << mPathFilters;
    }
    return data;
}

void QueryMessage::fromByteArray(const ByteArray &ba)
{
    Deserializer stream(ba.constData(), ba.size());
    stream >> mQuery >> mType >> mFlags >> mPathFilters;
}

unsigned QueryMessage::keyFlags(unsigned queryFlags)
{
    unsigned ret = RTags::NoFlag;
    if (!(queryFlags & QueryMessage::NoContext))
        ret |= RTags::ShowContext;
    if (queryFlags & QueryMessage::LineNumbers)
        ret |= RTags::ShowLineNumbers;
    return ret;
}

