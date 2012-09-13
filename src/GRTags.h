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
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onRecurseJobFinished(const Set<Path> &files);
    void recurse();
    void add(const Path &source);
    void onParseJobFinished(GRParseJob *job, const GRMap &data);
    void dirty(uint32_t fileId, GRMap &map);
private:
    weak_ptr<Project> mProject;
    FileSystemWatcher mWatcher;
    Map<uint32_t, GRParseJob*> mPending;
    int mActive, mCount;
};

#endif
