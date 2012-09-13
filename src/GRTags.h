#ifndef GRTags_h
#define GRTags_h

#include "Project.h"
#include "FileSystemWatcher.h"

class GRParseJob;
class GRTags
{
public:
    GRTags();
    void init(const shared_ptr<Project> &project);
    void onFileModified(const Path &path);
    void onFileRemoved(const Path &path);
    void onFileAdded(const Path &path);
    void onRecurseJobFinished(const Set<Path> &files);
    void recurse();
    void add(const Path &source);
    void onParseJobFinished(GRParseJob *job, const Map<ByteArray, Map<Location, bool> > &data);
private:
    weak_ptr<Project> mProject;
    FileSystemWatcher mWatcher;
};

#endif
