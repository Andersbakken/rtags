#include "FollowLocationJob.h"
#include "Rdm.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

FollowLocationJob::FollowLocationJob(int i, const Location &loc, unsigned f)
    : Job(i, QueryJobPriority), location(loc), flags(f)
{
}

FollowLocationJob::~FollowLocationJob()
{
}

void FollowLocationJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    CursorInfo cursorInfo = Rdm::findCursorInfo(db, location);
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
            const CursorInfo target = Rdm::findCursorInfo(db, cursorInfo.target);
            // error() << "cursorInfo is" << Rdm::eatString(clang_getCursorKindSpelling(cursorInfo.kind))
            //          << Rdm::eatString(clang_getCursorKindSpelling(target.kind));
            if (!target.isDefinition && !target.target.isNull()) {
                write(target.target.key(flags));
                return;
            }
            break; }
        }
        write(cursorInfo.target.key(flags));
    }
}
