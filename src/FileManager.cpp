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

#include "FileManager.h"
#include "ScanThread.h"
#include "Server.h"
#include "Filter.h"
#include "Project.h"

FileManager::FileManager()
    : mLastReloadTime(0)
{
    mWatcher.added().connect(std::bind(&FileManager::onFileAdded, this, std::placeholders::_1));
    mWatcher.removed().connect(std::bind(&FileManager::onFileRemoved, this, std::placeholders::_1));
    mScanTimer.timeout().connect(std::bind(&FileManager::startScanThread, this, std::placeholders::_1));
}

void FileManager::init(const std::shared_ptr<Project> &proj, Mode mode)
{
    mProject = proj;
    reload(mode);
}

void FileManager::reload(Mode mode)
{
    mLastReloadTime = Rct::monoMs();
    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    if (mode == Asynchronous) {
        mScanTimer.restart(5000, Timer::SingleShot);
    } else {
        const Set<Path> paths = ScanThread::paths(project->path());
        onRecurseJobFinished(paths);
    }
}

void FileManager::onRecurseJobFinished(const Set<Path> &paths)
{
    std::lock_guard<std::mutex> lock(mMutex); // ### is this needed now?

    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    FilesMap &map = project->files();
    map.clear();
    mWatcher.clear();
    for (Set<Path>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
        const Path parent = it->parentDir();
        if (parent.isEmpty()) {
            error() << "Got empty parent here" << *it;
            continue;
        }
        assert(!parent.isEmpty());
        Set<String> &dir = map[parent];
        watch(parent);
        dir.insert(it->fileName());
    }
    assert(!map.contains(""));
}

void FileManager::onFileAdded(const Path &path)
{
    // error() << "File added" << path;
    std::lock_guard<std::mutex> lock(mMutex);
    if (path.isEmpty()) {
        return;
    }
    const Filter::Result res = Filter::filter(path);
    switch (res) {
    case Filter::Directory:
        watch(path);
        reload(Asynchronous);
        return;
    case Filter::Filtered:
        return;
    default:
        break;
    }

    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    FilesMap &map = project->files();
    const Path parent = path.parentDir();
    if (!parent.isEmpty()) {
        Set<String> &dir = map[parent];
        watch(parent);
        dir.insert(path.fileName());
    } else {
        error() << "Got empty parent here" << path;
        reload(Asynchronous);
    }
    assert(!map.contains(Path()));
}

void FileManager::onFileRemoved(const Path &path)
{
    // error() << "File removed" << path;
    std::lock_guard<std::mutex> lock(mMutex);
    std::shared_ptr<Project> project = mProject.lock();
    FilesMap &map = project->files();
    if (map.contains(path)) {
        reload(Asynchronous);
        return;
    }
    const Path parent = path.parentDir();
    if (map.contains(parent)) {
        Set<String> &dir = map[parent];
        dir.remove(path.fileName());
        if (dir.isEmpty()) {
            mWatcher.unwatch(parent);
            map.remove(parent);
        }
    }
}

static inline bool startsWith(const Path &left, const Path &right)
{
    assert(!left.isEmpty());
    return !right.isEmpty() && left.startsWith(right);
}

bool FileManager::contains(const Path &path) const
{
    std::lock_guard<std::mutex> lock(mMutex);
    std::shared_ptr<Project> proj = mProject.lock();
    if (!proj)
        return false;
    if (startsWith(path, proj->path()))
        return true;
    const Path p = Path::resolved(path);
    if (p != path && startsWith(path, proj->path()))
        return true;
    return false;
}

void FileManager::watch(const Path &path)
{
    if (!(Server::instance()->options().options & Server::NoFileManagerWatch)
        && !path.contains("/.git/") && !path.contains("/.svn/") && !path.contains("/.cvs/")) {
        mWatcher.watch(path);
    }
}
void FileManager::startScanThread(Timer *)
{
    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    ScanThread *thread = new ScanThread(project->path());
    thread->setAutoDelete(true);
    thread->finished().connect<EventLoop::Move>(std::bind(&FileManager::onRecurseJobFinished, this, std::placeholders::_1));
    thread->start();
}
