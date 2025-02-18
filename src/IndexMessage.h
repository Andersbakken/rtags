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

#ifndef IndexMessage_h
#define IndexMessage_h

#include <utility>

#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/String.h"
#include "RTagsMessage.h"
#include "rct/Path.h"

class Deserializer;
class Serializer;

class IndexMessage : public RTagsMessage
{
public:
    enum { MessageId = CompileId };

    IndexMessage();

    const Path &projectRoot() const { return mProjectRoot; }
    void setProjectRoot(const Path &projectRoot) { mProjectRoot = projectRoot; }
    const Path &workingDirectory() const { return mWorkingDirectory; }
    void setWorkingDirectory(Path &&wd) { mWorkingDirectory = std::move(wd); }
    void setEnvironment(const List<String> &environment) { mEnvironment = environment; }
    const List<String> &environment() const { return mEnvironment; }
    List<String> &&takeEnvironment() { return std::move(mEnvironment); }
    Path compileCommands() const { return mCompileCommands; }
    void setCompileCommands(Path &&path) { mCompileCommands = std::move(path); }
    const String &arguments() const { return mArgs; }
    String &&takeArguments() { return std::move(mArgs); }
    void setArguments(String &&args) { mArgs = std::move(args); }
    enum Flag {
        None = 0x0,
        GuessFlags = 0x1
    };
    Flags<Flag> flags() const { return mFlags; }
    void setFlags(Flags<Flag> f) { mFlags = f; }
    void setFlag(Flag flag, bool on = true) { mFlags.set(flag, on); }
    virtual void encode(Serializer &serializer) const override;
    virtual void decode(Deserializer &deserializer) override;
private:
    Path mWorkingDirectory;
    String mArgs;
    List<String> mEnvironment;
    Path mProjectRoot;
    Path mCompileCommands;
    Flags<Flag> mFlags;
};

RCT_FLAGS(IndexMessage::Flag);

#endif
