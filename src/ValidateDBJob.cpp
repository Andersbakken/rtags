#include "ValidateDBJob.h"
#include "CursorInfo.h"
#include "RTags.h"
#include "Server.h"
#include "Project.h"
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
    {
        Scope<const SymbolMap&> scope = project()->lockSymbolsForRead();
        if (scope.isNull())
            return;
        const SymbolMap &map = scope.data();
        char *lastFileContents = 0;
        uint32_t lastFileId = -1;
        for (SymbolMap::const_iterator it = map.begin(); it != map.end(); ++it) {
            ++total;
            if (isAborted()) {
                delete []lastFileContents;
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
                    stream << " isDefinition: " << (ci.isDefinition() ? "true" : "false")
                           << " target: " << ci.targets
                           << " references:";
                    for (Set<Location>::const_iterator rit = ci.references.begin(); rit != ci.references.end(); ++rit) {
                        stream << " " << *rit;
                    }
                }
                newErrors.insert(loc);
                ++errors;
            } else if (it->second.kind != CXCursor_InclusionDirective) {
                if (lastFileId != it->first.fileId()) {
                    delete []lastFileContents;
                    lastFileId = it->first.fileId();
                    Location::path(lastFileId).readAll(lastFileContents);
                }
                int foundError = 0;
                int offset = it->first.offset();
                if (RTags::isOperator(lastFileContents[offset])) {
                    for (int i=1; i<it->second.symbolLength; ++i) {
                        if (!RTags::isOperator(lastFileContents[i + offset])) {
                            error() << "Found something wrong" << it->second.kind << lastFileContents[i + offset];
                            foundError = 1;
                            break;
                        }
                    }
                } else {
                    if (!strncmp(lastFileContents + offset, "operator", 8)) {
                        for (int i=8; i<it->second.symbolLength; ++i) {
                            if (!RTags::isOperator(lastFileContents[i + offset])) {
                                error() << "Found something wrong" << it->second.kind << lastFileContents[i + offset]
                                        << i << 2;
                                foundError = 2;
                                break;
                            }
                        }
                    } else {
                        for (int i=0; i<it->second.symbolLength; ++i) {
                            if (!RTags::isSymbol(lastFileContents[i + offset])) {
                                error() << "Found something wrong" << it->second.kind << lastFileContents[i + offset] << i
                                        << 3;
                                foundError = 3;
                                break;
                            }
                        }
                    }
                }
                if (!foundError) {
                    if (offset > 0 && RTags::isSymbol(lastFileContents[offset - 1])) {
                        foundError = 2;
                    } else if (RTags::isSymbol(lastFileContents[offset + it->second.symbolLength])) {
                        foundError = 3;
                    } else if (!ci.isValid(it->first)) {
                        foundError = 4;
                    }
                }
                if (foundError) {
                    error() << "Something is suspicious about" << foundError << it->first << it->second;
                    error() << String::format<64>("[%s]",
                                                  String(lastFileContents + offset - 1,
                                                         it->second.symbolLength + 2).constData());
                }

            }
        }
        delete []lastFileContents;
    }
    mErrors(newErrors);
    error("Checked %d CursorInfo objects, %d errors", total, errors);
}
