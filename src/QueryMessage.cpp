#include "QueryMessage.h"
#include "RTags.h"
#include <rct/Serializer.h>

QueryMessage::QueryMessage(Type type)
    : ClientMessage(MessageId), mType(type), mFlags(0), mMax(-1), mMinOffset(-1), mMaxOffset(-1), mBuildIndex(0)
{
}

void QueryMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mQuery << mContext << mType << mFlags << mMax
               << mMinOffset << mMaxOffset << mBuildIndex << mPathFilters << mProjects;
}

void QueryMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mQuery >> mContext >> mType >> mFlags >> mMax
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
