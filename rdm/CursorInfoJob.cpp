#include "CursorInfoJob.h"
#include "Rdm.h"
#include "leveldb/db.h"

CursorInfoJob::CursorInfoJob(int i, const RTags::Location &loc, unsigned f)
    : Job(i), location(loc), flags(f & ~RTags::Location::ShowContext)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

void CursorInfoJob::run()
{
    leveldb::DB *db = Server::instance()->db(Server::Symbol);
    RTags::Location found;
    const Rdm::CursorInfo cursorInfo = Rdm::findCursorInfo(db, location, &found);
    if (!cursorInfo.isNull()) {
        write(found.key(flags) + "\t" + cursorInfo.symbolName);
    }
    finish();
}
