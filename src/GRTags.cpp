#include "GRTags.h"
#include "Server.h"
#include "GRScanJob.h"
#include "GRParseJob.h"
#include <math.h>

GRTags::GRTags()
    : mActive(0), mCount(0)
{
    mWatcher.added().connect(this, &GRTags::onFileAdded);
    mWatcher.removed().connect(this, &GRTags::onFileRemoved);
    mWatcher.modified().connect(this, &GRTags::add);
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
    GRScanJob *job = new GRScanJob(GRScanJob::Sources, project->srcRoot(), project);
    job->finished().connect(this, &GRTags::onRecurseJobFinished);
    Server::instance()->threadPool()->start(shared_ptr<ThreadPool::Job>(job));
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
        if (it->second >= source.lastModified())
            return;
        flags = GRParseJob::Dirty;
    }
    shared_ptr<GRParseJob> job(new GRParseJob(source, flags, project));
    ++mActive;
    ++mCount;
    job->finished().connect(this, &GRTags::onParseJobFinished);
    Server::instance()->threadPool()->start(job);
}
void GRTags::onParseJobFinished(const shared_ptr<GRParseJob> &job, const GRMap &data)
{
    uint32_t fileId = Location::insertFile(job->path());
    const time_t time = job->parseTime();
    shared_ptr<Project> project = mProject.lock();
    {
        Scope<GRFilesMap&> scope = project->lockGRFilesForWrite();
        GRFilesMap &map = scope.data();
        time_t &ref = map[fileId];
        --mActive;
        if (ref >= time) {
            if (!mActive)
                mCount = 0;
            return;
        }
        ref = time;
        const int idx = mCount - mActive;
        if (idx % 50 == 0 || idx == mCount) {
            error("[%3d%%] Tagged %d/%d",
                  static_cast<int>(round((static_cast<double>(idx) / static_cast<double>(mCount)) * 100.0)), idx, mCount);
        }
        if (mActive == 0)
            mCount = 0;
    }
    {
        Scope<GRMap&> scope = project->lockGRForWrite();
        GRMap &map = scope.data();
        if (job->flags() & GRParseJob::Dirty)
            dirty(fileId, map);
        if (map.isEmpty()) {
            map = data;
        } else {
            for (GRMap::const_iterator it = data.begin(); it != data.end(); ++it) {
                Map<Location, bool> &existing = map[it->first];
                if (existing.isEmpty()) {
                    existing = it->second;
                } else {
                    existing.unite(it->second);
                }
            }
        }
    }
}

void GRTags::dirty(uint32_t fileId, GRMap &map)
{
    GRMap::iterator it = map.begin();
    while (it != map.end()) {
        Map<Location, bool> &val = it->second;
        Map<Location, bool>::iterator i = val.begin();
        while (i != val.end()) {
            if (i->first.fileId() == fileId) {
                val.erase(i++);
            } else {
                ++i;
            }
        }
        if (val.isEmpty()) {
            map.remove(it++->first);
        } else {
            ++it;
        }
    }
}

bool GRTags::isIndexed(uint32_t fileId) const
{
    return true; // what do here? Can I grab the GRLock from main thread?
}
