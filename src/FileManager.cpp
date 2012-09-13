#include "FileManager.h"
#include "GRScanJob.h"
#include "Server.h"
#include "Indexer.h"
#include "GRParseJob.h"
#include <math.h>

FileManager::FileManager()
    : mFilters(Server::instance()->excludeFilter())
{
    mWatcher.added().connect(this, &FileManager::onFileAdded);
    mWatcher.removed().connect(this, &FileManager::onFileRemoved);
}

void FileManager::init(const shared_ptr<Project> &proj)
{
    mProject = proj;
    recurseDirs();
}

void FileManager::recurseDirs()
{
    shared_ptr<Project> project = mProject.lock();
    assert(project);
    GRScanJob *job = new GRScanJob(project->srcRoot, mProject.lock());
    job->finished().connect(this, &FileManager::onRecurseJobFinished);
    Server::instance()->threadPool()->start(job);
}

void FileManager::onRecurseJobFinished(const Map<Path, bool> &paths)
{
    shared_ptr<Project> project = mProject.lock();
    assert(project);
    Scope<FilesMap&> scope = project->lockFilesForWrite();
    FilesMap &map = scope.data();
    mWatcher.clear();
    for (Map<Path, bool>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
        const Path parent = it->first.parentDir();
        Set<ByteArray> &dir = map[parent];
        if (dir.isEmpty())
            mWatcher.watch(parent);
        dir.insert(it->first.fileName());
    }
}

void FileManager::onFileAdded(const Path &path)
{
    const GRScanJob::FilterResult res = GRScanJob::filter(path, mFilters);
    switch (res) {
    case GRScanJob::Directory:
        recurseDirs();
        return;
    case GRScanJob::Filtered:
        return;
    default:
        break;
    }

    shared_ptr<Project> project = mProject.lock();
    assert(project);
    Scope<FilesMap&> scope = project->lockFilesForWrite();
    FilesMap &map = scope.data();
    const Path parent = path.parentDir();
    Set<ByteArray> &dir = map[parent];
    if (dir.isEmpty())
        mWatcher.watch(parent);
    dir.insert(path.fileName());
}

void FileManager::onFileRemoved(const Path &path)
{
    shared_ptr<Project> project = mProject.lock();
    Scope<FilesMap&> scope = project->lockFilesForWrite();
    FilesMap &map = scope.data();
    if (map.contains(path)) {
        recurseDirs();
        return;
    }
    const Path parent = path.parentDir();
    Set<ByteArray> &dir = map[parent];
    if (dir.remove(path.fileName()) && dir.isEmpty()) {
        mWatcher.unwatch(parent);
        map.remove(parent);
    }
}
