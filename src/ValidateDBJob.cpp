#include "ValidateDBJob.h"
#include "CursorInfo.h"
#include "Database.h"
#include "Indexer.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>

ValidateDBJob::ValidateDBJob(const Path &root, const Set<Location> &prev)
    : Job(0), mRoot(root), mPrevious(prev)
{
}

void ValidateDBJob::execute()
{
    ScopedDB db = Server::instance()->db(Server::Symbol, Server::Read, mRoot);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seekToFirst();
    int errors = 0;
    int total = 0;
    Set<Location> newErrors;
    while (it->isValid()) {
        ++total;
        if (isAborted())
            return;
        const CursorInfo ci = it->value<CursorInfo>();
        if (!ci.symbolLength) {
            const Location loc = Location::fromKey(it->key().data());
            if (!mPrevious.contains(loc)) {
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
            }
            newErrors.insert(loc);

            ++errors;
        }
        it->next();
    }
    mErrors(newErrors);
    error("Checked %d CursorInfo objects, %d errors", total, errors);
}
