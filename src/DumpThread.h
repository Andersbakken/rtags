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

#ifndef DumpThread_h
#define DumpThread_h

#include <clang-c/Index.h>

#include "Project.h"
#include "QueryMessage.h"
#include "rct/Thread.h"
#include "Source.h"

class Connection;
struct Dep;
class DumpThread : public Thread
{
public:
    DumpThread(const std::shared_ptr<QueryMessage> &queryMessage, const Source &source, const std::shared_ptr<Connection> &conn);
    virtual void run() override;
    void abort() { std::unique_lock<std::mutex> lock(mMutex); mAborted = false; }
    bool isAborted() const { std::unique_lock<std::mutex> lock(mMutex); return mAborted; }
private:
    static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData userData);
    CXChildVisitResult visit(const CXCursor &cursor);
    void checkIncludes(const Location &location, const CXCursor &cursor);

    void writeToConnetion(const String &message);
    Location createLocation(const CXSourceLocation &loc)
    {
        CXString fileName;
        unsigned int line, col;
        CXFile file;
        clang_getSpellingLocation(loc, &file, &line, &col, 0);
        if (file) {
            fileName = clang_getFileName(file);
        } else {
            return Location();
        }
        const char *fn = clang_getCString(fileName);
        assert(fn);
        if (!*fn || !strcmp("<built-in>", fn) || !strcmp("<command line>", fn)) {
            clang_disposeString(fileName);
            return Location();
        }
        Path path = RTags::eatString(fileName);
        uint32_t fileId = Location::fileId(path);
        if (!fileId) {
            path.resolve();
            fileId = Location::insertFile(path);
        }
        return Location(fileId, line, col);
    }
    Location createLocation(const CXCursor &cursor)
    {
        return createLocation(clang_getCursorLocation(cursor));
    }
    void handleInclude(const Location &loc, const CXCursor &cursor);
    void handleReference(const Location &loc, const CXCursor &ref);
    void checkIncludes();

    const Flags<QueryMessage::Flag> mQueryFlags;
    const Source mSource;
    std::shared_ptr<Connection> mConnection;
    int mIndentLevel;
    mutable std::mutex mMutex;
    Hash<uint32_t, Dep*> mDependencies;
    bool mAborted;
};

#endif
