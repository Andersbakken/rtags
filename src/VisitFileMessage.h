/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef VisitFileMessage_h
#define VisitFileMessage_h

#include "RTagsMessage.h"

class VisitFileMessage : public RTagsMessage
{
public:
    enum { MessageId = VisitFileId };

    VisitFileMessage(const Path &file = Path(), const Path &project = Path(), uint32_t sourceFileId = 0)
        : RTagsMessage(MessageId), mFile(file), mProject(project), mSourceFileId(sourceFileId)
    {
    }

    Path project() const { return mProject; }
    Path file() const { return mFile; }
    uint32_t sourceFileId() const { return mSourceFileId; }
    void encode(Serializer &serializer) const override { serializer << mProject << mFile << mSourceFileId; }
    void decode(Deserializer &deserializer) override { deserializer >> mProject >> mFile >> mSourceFileId; }
private:
    Path mFile, mProject;
    uint32_t mSourceFileId;
};

#endif
