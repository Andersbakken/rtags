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

#include "CompileMessage.h"
#include <rct/Serializer.h>

CompileMessage::CompileMessage()
    : RTagsMessage(MessageId), mEscape(false)
{
}

void CompileMessage::encode(Serializer &serializer) const
{
    serializer << mRaw << mWorkingDirectory << mProjectRoot << mCompilationDatabaseDir << mArgs << mEscape;
}

void CompileMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw >> mWorkingDirectory >> mProjectRoot >> mCompilationDatabaseDir >> mArgs >> mEscape;
}
