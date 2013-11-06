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

#ifndef CompileMessage_h
#define CompileMessage_h

#include <rct/List.h>
#include <rct/String.h>
#include "ClientMessage.h"
#include "RTags.h"

class CompileMessage : public ClientMessage
{
public:
    enum { MessageId = CompileId };

    CompileMessage(const Path &cwd = Path(), const String &args = String());

    const Path &workingDirectory() const { return mWorkingDirectory; }
    Path &&takeWorkingDirectory() { return std::move(mWorkingDirectory); }

    const String &arguments() const { return mArgs; }
    String &&takeArguments() { return std::move(mArgs); }
    void setArguments(const String &arguments) { mArgs = arguments; }

    void setProjects(const List<String> &projects) { mProjects = projects; }
    const List<String> &projects() const { return mProjects; }
    List<String> &&takeProjects() { return std::move(mProjects); }

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);
private:
    Path mWorkingDirectory;
    List<String> mProjects;
    String mArgs;
};

#endif
