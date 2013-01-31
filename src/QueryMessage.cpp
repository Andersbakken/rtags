#include "QueryMessage.h"
#include "RTags.h"
#include "Serializer.h"

QueryMessage::QueryMessage(Type type)
    : mType(type), mFlags(0), mMax(-1), mMinOffset(-1), mMaxOffset(-1), mBuildIndex(0)
{
}

ByteArray QueryMessage::encode() const
{
    ByteArray data;
    {
        Serializer stream(data);
        stream << mRaw << mQuery << mType << mFlags << mMax
               << mMinOffset << mMaxOffset << mBuildIndex << mPathFilters << mProjects;
    }
    return data;
}

void QueryMessage::fromData(const char *data, int size)
{
    Deserializer stream(data, size);
    stream >> mRaw >> mQuery >> mType >> mFlags >> mMax
           >> mMinOffset >> mMaxOffset >> mBuildIndex >> mPathFilters >> mProjects;
}

unsigned QueryMessage::keyFlags(unsigned queryFlags)
{
    unsigned ret = Location::NoFlag;
    if (!(queryFlags & QueryMessage::NoContext))
        ret |= Location::ShowContext;
    if (queryFlags & QueryMessage::LineNumbers)
        ret |= Location::ShowLineNumbers;
    return ret;
}

Match QueryMessage::match() const
{
    unsigned flags = Match::Flag_StringMatch;
    if (mFlags & MatchRegexp)
        flags |= Match::Flag_RegExp;

    return Match(mQuery, flags);
}
