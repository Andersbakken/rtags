#include "FollowLocationJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

FollowLocationJob::FollowLocationJob(const Location &loc, const QueryMessage &query)
    : Job(query, 0), location(loc)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, Server::Read);
    CursorInfo cursorInfo = RTags::findCursorInfo(db, location);
    if (isAborted())
        return;
    if (!cursorInfo.target.isNull()) {
        switch (cursorInfo.kind) {
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Destructor:
        case CXCursor_Constructor:
            break;
        default: {
            const CursorInfo target = RTags::findCursorInfo(db, cursorInfo.target);
            // error() << "cursorInfo is" << RTags::eatString(clang_getCursorKindSpelling(cursorInfo.kind))
            //          << RTags::eatString(clang_getCursorKindSpelling(target.kind));
            if (!target.isDefinition && !target.target.isNull()) {
                write(target.target.key(keyFlags()));
                return;
            }
            break; }
        }
        write(cursorInfo.target.key(keyFlags()));
    }
}
