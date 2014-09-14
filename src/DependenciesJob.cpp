/* This file is part of RTags.

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "DependenciesJob.h"
#include "RTags.h"
#include "Server.h"
#include "CursorInfo.h"
#include "FileManager.h"
#include "Project.h"

DependenciesJob::DependenciesJob(const std::shared_ptr<QueryMessage> &query, const std::shared_ptr<Project> &project)
    : QueryJob(query, QuietJob, project)
{
    Path p = query->query();
    p.resolve();
    mFileId = Location::fileId(p);
}

int DependenciesJob::execute()
{
    if (!mFileId)
        return 1;
    std::shared_ptr<Project> proj = project();
    if (!proj)
        return 2;
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
    return deps ? 0 : 3;
}
