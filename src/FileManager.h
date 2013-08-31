#ifndef FileManager_h
#define FileManager_h

#include <rct/Path.h>
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
    Set<Path> jsFiles() const;
    Signal<std::function<void()> > &jsFilesChanged() { return mJSFilesChanged; }

private:
    void watch(const Path &path);
    FileSystemWatcher mWatcher;
    std::weak_ptr<Project> mProject;
    Signal<std::function<void()> > mJSFilesChanged;
    Set<Path> mJSFiles;
    uint64_t mLastReloadTime;
    mutable std::mutex mMutex;
};

#endif
