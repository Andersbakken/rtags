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
#include "RTagsMessage.h"
#include "RTags.h"

class CompileMessage : public RTagsMessage
{
public:
    enum { MessageId = CompileId };

    CompileMessage();

    const Path &projectRoot() const { return mProjectRoot; }
    void setProjectRoot(const Path &projectRoot) { mProjectRoot = projectRoot; }
    const Path &workingDirectory() const { return mWorkingDirectory; }
    void setWorkingDirectory(const Path &workingDirectory) { mWorkingDirectory = workingDirectory; }
    Path compilationDatabaseDir() const { return mCompilationDatabaseDir; }
    void setCompilationDatabaseDir(const Path &path) { mCompilationDatabaseDir = path; }
    const String &arguments() const { return mArgs; }
    void setArguments(const String &arguments) { mArgs = arguments; }
    bool escape() const { return mEscape; }
    void setEscape(bool escape) { mEscape = escape; }

    virtual void encode(Serializer &serializer) const;
    virtual void decode(Deserializer &deserializer);
private:
    Path mWorkingDirectory;
    String mArgs;
    Path mProjectRoot;
    Path mCompilationDatabaseDir;
    bool mEscape;
};

#endif
