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

#ifndef FileManager_h
#define FileManager_h

#include <stdint.h>
#include <mutex>
#include <memory>

#include "rct/Path.h"
#include "rct/Timer.h"

class Project;
template <typename T> class Set;

class FileManager : public std::enable_shared_from_this<FileManager>
{
public:
    FileManager(const std::shared_ptr<Project> &project);
    enum Mode {
        Synchronous,
        Asynchronous
    };

    void load(Mode mode);
    uint64_t lastReloadTime() const { return mLastReloadTime; }
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(const Set<Path> &mPaths);
    bool contains(const Path &path) const;
    void clearFileSystemWatcher();
private:
    void startScanThread();
    void watch(const Path &path);
    std::weak_ptr<Project> mProject;
    uint64_t mLastReloadTime;
    mutable std::mutex mMutex;
};

#endif
