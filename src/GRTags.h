#ifndef GRTags_h
#define GRTags_h

#include "Path.h"
#include "List.h"
#include "FileSystemWatcher.h"
#include "Mutex.h"

class GRParseJob;
class Project;
class GRTags
{
public:
    GRTags();
    void init(const shared_ptr<Project> &proj);
    void recurseDirs();
    void onDirectoryModified(const Path &path);
    void onRecurseJobFinished(Map<Path, bool> &mPaths);
    void onParseJobFinished(GRParseJob *job, const Map<ByteArray, Map<Location, bool> > &entries);
    void remove(const Path &file, ScopedDB *grfiles = 0);
    void dirty(uint32_t fileId, ScopedDB &db);
    void parse(const Path &path, unsigned flags);
private:
    Map<Path, Map<ByteArray, time_t> > mFiles; // key: dir, value: fileName, last modified
    FileSystemWatcher *mWatcher;
    weak_ptr<Project> mProject;
    Path mSrcRoot;
    friend class FindFileJob;
    Mutex mMutex;
    int mCount, mActive;
};

#endif
