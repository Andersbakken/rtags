#include "CursorInfoJob.h"
#include "Rdm.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(int i, const Location &loc, unsigned f)
    : Job(i, QueryJobPriority), location(loc), flags(f & ~RTags::ShowContext)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

void CursorInfoJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ScopedDB::Read);
    Location found;
    const CursorInfo cursorInfo = Rdm::findCursorInfo(db, location, &found);
    if (isAborted())
        return;
    if (!cursorInfo.isNull()) {
        char buf[1024];
        const CXStringScope kind(clang_getCursorKindSpelling(cursorInfo.kind));
        const int w = snprintf(buf, sizeof(buf), "%s symbolName: '%s' symbolLength: %d type: %s",
                               found.key(flags).constData(), cursorInfo.symbolName.constData(),
                               cursorInfo.symbolLength, clang_getCString(kind.string));
        write(QByteArray(buf, w));
    }
}
