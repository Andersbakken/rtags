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
    void recurseDirs();
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(const Set<Path> &mPaths);
    bool contains(const Path &path) const;
    void reload();
    Set<Path> jsFiles() const;
    signalslot::Signal0 &jsFilesChanged() { return mJSFilesChanged; }
private:
    FileSystemWatcher mWatcher;
    weak_ptr<Project> mProject;
    signalslot::Signal0 mJSFilesChanged;
    Set<Path> mJSFiles;
    mutable Mutex mMutex;
};

#endif
