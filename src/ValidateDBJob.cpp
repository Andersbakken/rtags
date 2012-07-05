#include "ValidateDBJob.h"
#include "CursorInfo.h"
#include "Database.h"
#include "Indexer.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>

ValidateDBJob::ValidateDBJob()
    : Job(-1, ValidateDBJobPriority, 0)
{
}

void ValidateDBJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, ReadWriteLock::Read);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seekToFirst();
    int errors = 0;
    int total = 0;
    while (it->isValid()) {
        ++total;
        if (isAborted())
            return;
        const CursorInfo ci = it->value<CursorInfo>();
        if (!ci.symbolLength) {
            const Location loc = Location::fromKey(it->key().data());
            Log stream(Error);
            stream << "Invalid entry for " << loc
                   << " symbolName: " << ci.symbolName
                   << " kind: " << RTags::eatString(clang_getCursorKindSpelling(ci.kind))
                   << " isDefinition: " << (ci.isDefinition ? "true" : "false")
                   << " target: " << ci.target
                   << " references:";
            for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
                stream << " " << *rit;
            }

            ++errors;
        }
        it->next();
    }
    error("Checked %d cursors info objects, %d errors", total, errors);
}
