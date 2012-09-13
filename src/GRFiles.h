#ifndef GRFiles_h
#define GRFiles_h

#include "Path.h"
#include "List.h"
#include "FileSystemWatcher.h"
#include "Mutex.h"
#include "Location.h"

class Project;
class GRFiles
{
public:
    GRFiles();
    void init(const shared_ptr<Project> &proj);
    void recurseDirs();
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(const Map<Path, bool> &mPaths);
private:
    const List<ByteArray> &mFilters;
    FileSystemWatcher mWatcher;
    weak_ptr<Project> mProject;
};

#endif
