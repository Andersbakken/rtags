
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

#include <rct/Thread.h>
#include <Source.h>
#include <QueryMessage.h>
#include <clang-c/Index.h>

class Connection;
class DumpThread : public Thread
{
public:
    DumpThread(const std::shared_ptr<QueryMessage> &queryMessage, const Source &source, const std::shared_ptr<Connection> &conn);
    virtual void run() override;
private:
    static CXChildVisitResult visitor(CXCursor cursor, CXCursor, CXClientData userData);
    void writeToConnetion(const String &message);
    const Flags<QueryMessage::Flag> mQueryFlags;
    const Source mSource;
    std::shared_ptr<Connection> mConnection;
    Hash<Path, uint32_t> mFiles;
    int mIndentLevel;
};

#endif
