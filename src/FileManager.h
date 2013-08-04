#ifndef FileManager_h
#define FileManager_h

#include <rct/Path.h>
#include <rct/List.h>
#include <rct/FileSystemWatcher.h>
#include <rct/Mutex.h>
#include "Location.h"

class Project;
class FileManager
{
public:
    FileManager();
    void init(const shared_ptr<Project> &proj);
    void reload();
    uint64_t lastReloadTime() const { return mLastReloadTime; }
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(Set<Path> mPaths);
    bool contains(const Path &path) const;
    void clearFileSystemWatcher() { mWatcher.clear(); }
    Set<Path> watchedPaths() const { return mWatcher.watchedPaths(); }
    Set<Path> jsFiles() const;
    Signal<std::function<void()> > &jsFilesChanged() { return mJSFilesChanged; }

private:
    void watch(const Path &path);
    FileSystemWatcher mWatcher;
    weak_ptr<Project> mProject;
    Signal<std::function<void()> > mJSFilesChanged;
    Set<Path> mJSFiles;
    uint64_t mLastReloadTime;
    mutable Mutex mMutex;
};

#endif
