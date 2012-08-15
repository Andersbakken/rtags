#include "ValidateDBJob.h"
#include "CursorInfo.h"
#include "Database.h"
#include "Indexer.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>

ValidateDBJob::ValidateDBJob(const shared_ptr<Project> &proj, const Set<Location> &prev)
    : Job(0, proj), mPrevious(prev)
{
}

void ValidateDBJob::execute()
{
    int errors = 0;
    int total = 0;
    Set<Location> newErrors;

    ScopedDB db = project()->db(Project::Symbol, ReadWriteLock::Read);
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seekToFirst();
    while (it->isValid()) {
        ++total;
        if (isAborted()) {
            return;
        }
        const CursorInfo ci = it->value<CursorInfo>();
        if (!ci.symbolLength) {
            assert(it->key().size() == sizeof(uint64_t));
            const Location loc = Location::fromKey(it->key().data());
            if (!mPrevious.contains(loc)) {
                Log stream(Error);
                stream << "Invalid entry for " << loc
                       << " symbolName: " << ci.symbolName;
                if (ci.kind)
                    stream << " kind: " << RTags::eatString(clang_getCursorKindSpelling(ci.kind));// this somehow seems to hang when kind == 0
                stream << " isDefinition: " << (ci.isDefinition ? "true" : "false")
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
