#include "CursorInfoJob.h"
#include "Rdm.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(int i, const Location &loc, unsigned f)
    : Job(i), location(loc), flags(f & ~RTags::ShowContext)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

void CursorInfoJob::execute()
{
    Database *db = Server::instance()->db(Server::Symbol);
    Location found;
    const CursorInfo cursorInfo = Rdm::findCursorInfo(db, location, &found);
    if (isAborted())
        return;
    if (!cursorInfo.isNull()) {
        write(found.key(flags) + "\t" + cursorInfo.symbolName);
    }
}
