/* This file is part of RTags (http://rtags.net).

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

#ifndef FileManager_h
#define FileManager_h

#include <rct/Path.h>
#include <rct/Timer.h>
#include <rct/List.h>
#include <rct/FileSystemWatcher.h>
#include "Location.h"
#include <mutex>

class Project;
class FileManager
{
public:
    FileManager();
    enum Mode {
        Synchronous,
        Asynchronous
    };

    void init(const std::shared_ptr<Project> &proj, Mode mode);
    void reload(Mode mode);
    uint64_t lastReloadTime() const { return mLastReloadTime; }
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(const Set<Path> &mPaths);
    bool contains(const Path &path) const;
    void clearFileSystemWatcher() { mWatcher.clear(); }
    Set<Path> watchedPaths() const { return mWatcher.watchedPaths(); }
private:
    void startScanThread(Timer *);
    void watch(const Path &path);
    Timer mScanTimer;
    FileSystemWatcher mWatcher;
    std::weak_ptr<Project> mProject;
    uint64_t mLastReloadTime;
    mutable std::mutex mMutex;
};

#endif
