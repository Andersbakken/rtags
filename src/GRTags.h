#ifndef GRTags_h
#define GRTags_h

#include "Path.h"
#include "List.h"
#include "FileSystemWatcher.h"

class GRTags
{
public:
    GRTags(const Path &srcRoot);
    void recurseDirs();
    void onRecurseJobFinished(const List<Path> &mPaths);
private:
    const Path mSrcRoot;
    FileSystemWatcher *mWatcher;
};

#endif
