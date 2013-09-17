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

#ifndef CompletionMessage_h
#define CompletionMessage_h

#include "ClientMessage.h"
#include <rct/Path.h>
#include <rct/String.h>

class CompletionMessage : public ClientMessage
{
public:
    enum { MessageId = CompletionId };

    enum Flag {
        None = 0x0,
        Stream = 0x1
    };

    CompletionMessage(unsigned flags = 0, const Path &path = Path(), int line = -1, int column = -1, int pos = -1);
    unsigned flags() const { return mFlags; }
    Path path() const { return mPath; }
    int line() const { return mLine; }
    int column() const { return mColumn; }
    int pos() const { return mPos; }

    void setContents(const String &contents) { mContents = contents; }
    String contents() const { return mContents; }

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);

    void setProjects(const List<String> &projects) { mProjects = projects; }
    List<String> projects() const { return mProjects; }
private:
    unsigned mFlags;
    Path mPath;
    int mLine, mColumn, mPos;
    String mContents;
    List<String> mProjects;
};

#endif
