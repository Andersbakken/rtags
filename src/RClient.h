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

#ifndef RClient_h
#define RClient_h

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Map.h>
#include <rct/Path.h>
#include "QueryMessage.h"

class RCCommand;
class QueryCommand;
class Connection;
class RClient
{
public:
    RClient();
    ~RClient();
    bool exec();
    bool parse(int &argc, char **argv);

    int max() const { return mMax; }
    int logLevel() const { return mLogLevel; }
    int timeout() const { return mTimeout; }

    const Set<String> &pathFilters() const { return mPathFilters; }
    int minOffset() const { return mMinOffset; }
    int maxOffset() const { return mMaxOffset; }

    const Map<Path, String> &unsavedFiles() const { return mUnsavedFiles; }

    const List<String> &rdmArgs() const { return mRdmArgs; }
    const List<String> &projects() const { return mProjects; }

    String socketFile() const { return mSocketFile; }

    String context() const { return mContext; }

    unsigned queryFlags() const { return mQueryFlags; }

    int argc() const { return mArgc; }
    char **argv() const { return mArgv; }
    void onNewMessage(const Message *message, Connection *);
private:
    void addQuery(QueryMessage::Type t, const String &query = String());

    void addLog(int level);
    void addCompile(const Path &cwd, const String &args);

    unsigned mQueryFlags;
    int mMax, mLogLevel, mTimeout, mMinOffset, mMaxOffset, mConnectTimeout;
    String mContext;
    Set<String> mPathFilters;
    Map<Path, String> mUnsavedFiles;
    List<std::shared_ptr<RCCommand> > mCommands;
    List<String> mRdmArgs;
    String mSocketFile;
    List<String> mProjects;

    int mArgc;
    char **mArgv;
};

#endif

