/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "QueryMessage.h"
#include "RTags.h"
#include <rct/Serializer.h>

QueryMessage::QueryMessage(Type type)
    : RTagsMessage(MessageId), mType(type), mFlags(0), mMax(-1), mMinLine(-1), mMaxLine(-1), mBuildIndex(0)
{
}

void QueryMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mQuery << mType << mFlags << mMax
               << mMinLine << mMaxLine << mBuildIndex << mPathFilters << mCurrentFile
               << mUnsavedFiles;
}

void QueryMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mQuery >> mType >> mFlags >> mMax
                 >> mMinLine >> mMaxLine >> mBuildIndex >> mPathFilters >> mCurrentFile
                 >> mUnsavedFiles;
}

unsigned QueryMessage::keyFlags(unsigned queryFlags)
{
    unsigned ret = Location::NoFlag;
    if (!(queryFlags & QueryMessage::NoContext))
        ret |= Location::ShowContext;
    return ret;
}

Match QueryMessage::match() const
{
    unsigned flags = Match::Flag_StringMatch;
    if (mFlags & MatchRegexp)
        flags |= Match::Flag_RegExp;

    return Match(mQuery, flags);
}
