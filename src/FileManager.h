#ifndef FileManager_h
#define FileManager_h

#include <rct/Path.h>
#include <rct/List.h>
#include <rct/FileSystemWatcher.h>
#include <rct/Mutex.h>
#include <rct/EventReceiver.h>
#include "Location.h"

class Project;
class FileManager : public EventReceiver
{
public:
    FileManager();
    void init(const shared_ptr<Project> &proj);
    void reload();
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(Set<Path> mPaths);
    bool contains(const Path &path) const;
    void clearFileSystemWatcher() { mWatcher.clear(); }
    Set<Path> watchedPaths() const { return mWatcher.watchedPaths(); }
    Set<Path> jsFiles() const;
    signalslot::Signal0 &jsFilesChanged() { return mJSFilesChanged; }
private:
    void watch(const Path &path);
    FileSystemWatcher mWatcher;
    weak_ptr<Project> mProject;
    signalslot::Signal0 mJSFilesChanged;
    Set<Path> mJSFiles;
    mutable Mutex mMutex;
};

#endif
