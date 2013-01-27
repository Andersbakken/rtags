#include "JSONJob.h"
#include "CursorInfo.h"
#include "RTags.h"
#include "Server.h"

JSONJob::JSONJob(const QueryMessage &q, const shared_ptr<Project> &project)
    : Job(q, WriteUnfiltered, project), match(q.match()), mSymbolCount(0), mFileCount(0)
{
    assert(project.get());
}

void JSONJob::execute()
{
    shared_ptr<Project> proj = project();
    const Path root = proj->path();
    const DependencyMap deps = proj->dependencies();;
    // error() << deps.keys();
    assert(proj);
    Scope<const SymbolMap&> scope = proj->lockSymbolsForRead();
    assert(!scope.isNull());
    write("{");
    for (DependencyMap::const_iterator it = deps.begin(); it != deps.end(); ++it) {
        const Path path = Location::path(it->first);
        if (path.startsWith(root) && (match.isEmpty() || match.match(path))) {
            processFile(it->first, path.mid(root.size()), scope.data());
        }
    }
    write("}");
}

static ByteArray toJSON(const Location &loc, int length, int srcRootLength)
{
    return ByteArray::format<64>("{ \"file\": \"%s\", \"offset\": %d, \"length\": %d }",
                                 Location::path(loc.fileId()).constData() + srcRootLength, loc.offset(), length);

}

void JSONJob::processFile(uint32_t fileId, const Path &path, const SymbolMap &map)
{
    const Location loc(fileId, 0);
    SymbolMap::const_iterator it = map.lower_bound(loc);
    const int srcRootLength = project()->path().size();
    bool found = false;
    while (it != map.end() && it->first.fileId() == fileId) {
        if (!found) {
            write<64>("\"%s\": [", path.constData());
            found = true;
        }
        Location targetLocation;
        CursorInfo target = it->second.bestTarget(map, &targetLocation);
        const CXStringScope type = clang_getCursorKindSpelling(it->second.kind);
        if (!targetLocation.isNull()) {
            write<256>(", { \"location\": %s, \"type\": \"%s\", \"target\": %s }",
                       toJSON(it->first, it->second.symbolLength, srcRootLength).constData(), type.data(),
                       toJSON(targetLocation, target.symbolLength, srcRootLength).constData());
        } else {
            write<256>(", { \"location\": %s, \"type\": \"%s\" }",
                       toJSON(it->first, it->second.symbolLength, srcRootLength).constData(), type.data());
        }

        ++it;
    }
    if (found) {
        write("], ");
    }
}
