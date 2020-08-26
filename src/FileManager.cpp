/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#include "FileManager.h"

#include <assert.h>
#include <functional>
#include <set>

#include "Filter.h"
#include "Project.h"
#include "ScanThread.h"
#include "Server.h"
#include "RTags.h"
#include "rct/EventLoop.h"
#include "rct/Flags.h"
#include "rct/List.h"
#include "rct/Log.h"
#include "rct/Rct.h"
#include "rct/Set.h"
#include "rct/SignalSlot.h"
#include "rct/String.h"

FileManager::FileManager(const std::shared_ptr<Project> &project)
    : mProject(project), mLastReloadTime(0)
{
}

void FileManager::load(Mode mode)
{
    if (!Server::instance()->options().tests.isEmpty())
        mode = Synchronous;

    mLastReloadTime = Rct::monoMs();
    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    if (mode == Asynchronous) {
        startScanThread();
    } else {
        const Set<Path> paths = ScanThread::paths(project->path());
        onRecurseJobFinished(paths);
    }
}

void FileManager::onRecurseJobFinished(const Set<Path> &paths)
{
    std::lock_guard<std::mutex> lock(mMutex); // ### is this needed now?

    std::shared_ptr<Project> project = mProject.lock();
    if (!project)
        return;
    Files &map = project->files();
    map.clear();
    clearFileSystemWatcher();
    for (Set<Path>::const_iterator it = paths.begin(); it != paths.end(); ++it) {
        const Path parent = it->parentDir();
        if (parent.isEmpty()) {
            error() << "Got empty parent here" << *it;
            continue;
        }
        assert(!parent.isEmpty());
        Set<String> &dir = map[parent];
        if (dir.isEmpty()) {
            watch(parent);
            // error() << "Watching parent" << parent;
        }
        dir.insert(it->fileName());
    }
    assert(!map.contains(Path()));
}

void FileManager::onFileAdded(const Path &path)
{
    debug() << "fm file added" << path;
    std::lock_guard<std::mutex> lock(mMutex);
    if (path.isEmpty()) {
        return;
    }
    const Filter::Result res = Filter::filter(path);
    switch (res) {
    case Filter::Directory:
        watch(path);
        load(Asynchronous);
        return;
    case Filter::Filtered:
        return;
    default:
        break;
    }

    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    Files &map = project->files();
    const Path parent = path.parentDir();
    if (!parent.isEmpty()) {
        Set<String> &dir = map[parent];
        watch(parent);
        dir.insert(path.fileName());
    } else {
        error() << "Got empty parent here" << path;
        load(Asynchronous);
    }
    assert(!map.contains(Path()));
}

void FileManager::onFileRemoved(const Path &path)
{
    if (path.exists())
        return;
    debug() << "fm file removed" << path;
    std::lock_guard<std::mutex> lock(mMutex);
    std::shared_ptr<Project> project = mProject.lock();
    if (!project)
        return;
    Files &map = project->files();
    if (!map.remove(path)) {
        const Path parent = path.parentDir();
        if (map.contains(parent)) {
            Set<String> &dir = map[parent];
            dir.remove(String(path.fileName()));
            if (dir.isEmpty()) {
                project->unwatch(parent, Project::Watch_FileManager);
                map.remove(parent);
            }
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
    if (Server::instance()->options().options & Server::NoFileManagerWatch)
        return;
    if (path.contains("/.git/") || path.contains("/.svn/") || path.contains("/.cvs/")) {
        return; // more source control systems?
    }
    if (auto proj = mProject.lock()) {
        proj->watch(path, Project::Watch_FileManager);
    }
}

void FileManager::startScanThread()
{
    std::shared_ptr<Project> project = mProject.lock();
    assert(project);
    ScanThread *thread = new ScanThread(project->path());
    thread->setAutoDelete(true);
    std::weak_ptr<FileManager> that = shared_from_this();
    thread->finished().connect<EventLoop::Move>([that](const Set<Path> &paths) {
            if (auto strong = that.lock())
                strong->onRecurseJobFinished(paths);
        });

    thread->start();
}

void FileManager::clearFileSystemWatcher()
{
    if (auto project = mProject.lock())
        project->clearWatch(Project::Watch_FileManager);
}
