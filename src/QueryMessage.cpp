#include "QueryMessage.h"
#include "RTags.h"
#include <Serializer.h>

QueryMessage::QueryMessage(Type type, const ByteArray& query, unsigned flags)
    : mType(type), mFlags(flags)
{
    mQuery.append(query);
}

ByteArray QueryMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mQuery << mType << mFlags << mPathFilters;
    }
    return data;
}

void QueryMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
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

