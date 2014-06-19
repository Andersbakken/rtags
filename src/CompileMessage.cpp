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
    : RTagsMessage(MessageId), mEscape(false), mBuildIndex(0)
{
}

enum { Compile = 1, Update };
void CompileMessage::encode(Serializer &serializer) const
{
    serializer << mRaw;
    if (mSourceFile.isEmpty()) {
        serializer << static_cast<uint8_t>(Compile)
                   << mWorkingDirectory << mProjectRoot << mCompilationDatabaseDir
                   << mArgs << mEscape;
    } else {
        serializer << static_cast<uint8_t>(Update)
                   << mSourceFile << mBuildIndex << mContents;
    }
}

void CompileMessage::decode(Deserializer &deserializer)
{
    deserializer >> mRaw;
    uint8_t type;
    deserializer >> type;
    switch (type) {
    case Compile:
        deserializer >> mWorkingDirectory >> mProjectRoot >> mCompilationDatabaseDir >> mArgs >> mEscape;
        break;
    case Update:
        deserializer >> mSourceFile >> mBuildIndex >> mContents;
        break;
    default:
        error() << "Invalid CompileMessage" << type;
        break;
    }
}
