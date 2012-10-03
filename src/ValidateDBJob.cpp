#include "ValidateDBJob.h"
#include "CursorInfo.h"
#include "Indexer.h"
#include "RTags.h"
#include "Server.h"
#include <clang-c/Index.h>

ValidateDBJob::ValidateDBJob(const std::shared_ptr<Project> &proj, const Set<Location> &prev)
    : Job(0, proj), mPrevious(prev)
{
}

void ValidateDBJob::execute()
{
    int errors = 0;
    int total = 0;
    Set<Location> newErrors;

    Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
    if (scope.isNull())
        return;
    const SymbolMap &map = scope.data();
    for (SymbolMap::const_iterator it = map.begin(); it != map.end(); ++it) {
        if (isAborted()) {
            return;
        }
        const CursorInfo &ci = it->second;
        if (!ci.symbolLength) {
            const Location &loc = it->first;
            if (!mPrevious.contains(loc)) {
                Log stream(Error);
                stream << "Invalid entry for " << loc
                       << " symbolName: " << ci.symbolName;
                if (ci.kind)
                    stream << " kind: " << RTags::eatString(clang_getCursorKindSpelling(ci.kind));// this somehow seems to hang when kind == 0
                stream << " isDefinition: " << (ci.isDefinition ? "true" : "false")
                       << " target: " << ci.targets
                       << " references:";
                for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
                    stream << " " << *rit;
                }
            }
            newErrors.insert(loc);
            ++errors;
        }
    }
    mErrors(newErrors);
    error("Checked %d CursorInfo objects, %d errors", total, errors);
}
