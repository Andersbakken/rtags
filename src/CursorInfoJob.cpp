#include "CursorInfoJob.h"
#include "RTags.h"
#include "ScopedDB.h"
#include "Server.h"
#include "leveldb/db.h"
#include "CursorInfo.h"

CursorInfoJob::CursorInfoJob(int i, const Location &loc, unsigned f)
    : Job(i, QueryJobPriority), location(loc), flags(f & ~Location::ShowContext)
{
}

CursorInfoJob::~CursorInfoJob()
{
}

void CursorInfoJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Read);
    Location found;
    const CursorInfo ci = RTags::findCursorInfo(db, location, &found);
    if (isAborted())
        return;
    if (ci.symbolLength) {
        char buf[1024];
        const CXStringScope kind(clang_getCursorKindSpelling(ci.kind));
        const int w = snprintf(buf, sizeof(buf), "%s symbolName: %s kind: %s isDefinition: %s symbolLength: %d%s",
                               found.key(flags).constData(), ci.symbolName.constData(),
                               clang_getCString(kind.string), ci.isDefinition ? "true" : "false",
                               ci.symbolLength,
                               (ci.references.isEmpty() && ci.additionalReferences.isEmpty() ? "" : " references:"));
        write(ByteArray(buf, w));
        for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
            const Location &l = *rit;
            snprintf(buf, sizeof(buf), "    %s", l.key().constData());
            write(buf);
        }
        for (Set<Location>::const_iterator rit = ci.additionalReferences.begin(); rit != ci.additionalReferences.end(); ++rit) {
            const Location &l = *rit;
            snprintf(buf, sizeof(buf), "    %s (additional)", l.key().constData());
            write(buf);
        }
    }
}
