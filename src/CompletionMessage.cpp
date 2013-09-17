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
