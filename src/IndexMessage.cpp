/* This file is part of RTags (http://rtags.net).

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

#include "IndexMessage.h"
#include <rct/Serializer.h>

IndexMessage::IndexMessage()
    : RTagsMessage(MessageId), mFlags(None)
{
}

void IndexMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mWorkingDirectory << mProjectRoot << mCompilationDatabaseDir << mArgs << mFlags;
}

void IndexMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mWorkingDirectory >> mProjectRoot >> mCompilationDatabaseDir >> mArgs >> mFlags;
}
