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

#ifndef IndexMessage_h
#define IndexMessage_h

#include <rct/List.h>
#include <rct/String.h>
#include "RTagsMessage.h"
#include "RTags.h"
#include <rct/Flags.h>

class IndexMessage : public RTagsMessage
{
public:
    enum { MessageId = CompileId };

    IndexMessage();

    const Path &projectRoot() const { return mProjectRoot; }
    void setProjectRoot(const Path &projectRoot) { mProjectRoot = projectRoot; }
    const Path &workingDirectory() const { return mWorkingDirectory; }
    void setWorkingDirectory(const Path &workingDirectory) { mWorkingDirectory = workingDirectory; }
    Path compilationDatabaseDir() const { return mCompilationDatabaseDir; }
    void setCompilationDatabaseDir(const Path &path) { mCompilationDatabaseDir = path; }
    const String &arguments() const { return mArgs; }
    void setArguments(const String &arguments) { mArgs = arguments; }
    enum Flag {
        None = 0x0,
        Escape = 0x1,
        GuessFlags = 0x2
    };
    Flags<Flag> flags() const { return mFlags; }
    void setFlags(Flags<Flag> flags) { mFlags = flags; }
    void setFlag(Flag flag, bool on = true) { mFlags.set(flag, on); }
    virtual void encode(Serializer &serializer) const override;
    virtual void decode(Deserializer &deserializer) override;
private:
    Path mWorkingDirectory;
    String mArgs;
    Path mProjectRoot;
    Path mCompilationDatabaseDir;
    Flags<Flag> mFlags;
};

RCT_FLAGS(IndexMessage::Flag);

#endif
