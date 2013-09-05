#include "DependenciesJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "FileManager.h"
#include "Project.h"

DependenciesJob::DependenciesJob(const QueryMessage &query, const std::shared_ptr<Project> &project)
    : Job(query, WriteBuffered|QuietJob, project), mFileId(Location::fileId(query.query()))
{
}

void DependenciesJob::execute()
{
    if (!mFileId)
        return;
    std::shared_ptr<Project> proj = project();
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
    bool deps = false;
    if (!dependencies.isEmpty()) {
        deps = true;
        write<64>("%s is depended on by:", path.constData());
        for (Set<uint32_t>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            write<64>("  %s", Location::path(*it).constData());
        }
    }
    dependencies = proj->dependencies(mFileId, Project::ArgDependsOn);
    dependencies.remove(mFileId);
    if (!dependencies.isEmpty()) {
        deps = true;
        write<64>("%s depends on:", path.constData());
        for (Set<uint32_t>::const_iterator it = dependencies.begin(); it != dependencies.end(); ++it) {
            write<64>("  %s", Location::path(*it).constData());
        }
    }
    if (!deps) {
        write<64>("%s has no dependencies", path.constData());
    }
}
