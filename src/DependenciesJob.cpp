#include "DependenciesJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "FileManager.h"

DependenciesJob::DependenciesJob(const QueryMessage &query, const shared_ptr<Project> &project)
    : Job(query, WriteBuffered, project), mFileId(Location::fileId(query.query()))
{
}

void DependenciesJob::execute()
{
    if (!mFileId)
        return;
    shared_ptr<Project> proj = project();
    if (!proj)
        return;
    const Path srcRoot = proj->path();

    error() << (queryFlags() & QueryMessage::FilterSystemIncludes);
    // const bool absolute = (queryFlags() & QueryMessage::AbsolutePath);
    Set<uint32_t> dependencies = proj->dependencies(mFileId, Project::DependsOnArg);
    dependencies.remove(mFileId);
    Path path = Location::path(mFileId);
    // if (!absolute && path.startsWith(srcRoot))
    //     absolute.remove(0, srcRoot.size());
    if (!dependencies.isEmpty()) {
        write<64>("%s is depended on by:", path.constData());
        for (Set<uint32_t>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            write<64>("  %s", Location::path(*it).constData());
        }
    }
    dependencies = proj->dependencies(mFileId, Project::ArgDependsOn);
    if (!dependencies.isEmpty()) {
        write<64>("%s depends on:", path.constData());
        for (Set<uint32_t>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            write<64>("  %s", Location::path(*it).constData());
        }
    }
}
