#include "GRTags.h"
#include "Server.h"
#include "GRScanJob.h"
#include "GRParseJob.h"

GRTags::GRTags()
{
    mWatcher.added().connect(this, &GRTags::onFileAdded);
    mWatcher.removed().connect(this, &GRTags::onFileRemoved);
    mWatcher.removed().connect(this, &GRTags::onFileModified);
}

void GRTags::init(const shared_ptr<Project> &project)
{
    assert(project);
    mProject = project;
    recurse();
}

void GRTags::recurse()
{
    shared_ptr<Project> project = mProject.lock();
    GRScanJob *job = new GRScanJob(GRScanJob::Sources, project->srcRoot, project);
    job->finished().connect(this, &GRTags::onRecurseJobFinished);
    Server::instance()->threadPool()->start(job);
}

void GRTags::onFileModified(const Path &path)
{

}

void GRTags::onFileRemoved(const Path &path)
{
    // how do I know that a directory was removed?
}

void GRTags::onFileAdded(const Path &path)
{
    const GRScanJob::FilterResult res = GRScanJob::filter(path, Server::instance()->excludeFilter());
    switch (res) {
    case GRScanJob::Directory:
        recurse();
        break;
    case GRScanJob::Source:
        add(path);
        break;
    default:
        break;
    }
}

void GRTags::onRecurseJobFinished(const Set<Path> &files)
{
    for (Set<Path>::const_iterator it = files.begin(); it != files.end(); ++it)
        add(*it);
}

void GRTags::add(const Path &source)
{
    shared_ptr<Project> project = mProject.lock();
    assert(project);
    const uint32_t fileId = Location::insertFile(source);
    Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
    GRFilesMap &map = scope.data();
    GRFilesMap::const_iterator it = map.find(fileId);
    unsigned flags = 0;
    if (it != map.end()) {
        if (!it->second)
            return;
        flags = GRParseJob::Dirty;
    }
    map[fileId] = 0;
    GRParseJob *job = new GRParseJob(source, flags, project);
    job->finished().connect(this, &GRTags::onParseJobFinished);
    Server::instance()->threadPool()->start(job);
}
void GRTags::onParseJobFinished(GRParseJob *job, const Map<ByteArray, Map<Location, bool> > &data)
{
    // need to dirty existing if job->flags() & Dirty, stick it into mGR and update GRFiles with parsetime
}
