#ifndef GRTags_h
#define GRTags_h

#include "Path.h"
#include "List.h"
#include "FileSystemWatcher.h"
#include "Mutex.h"

class GRParseJob;
class GRTags
{
public:
    GRTags(const Path &srcRoot);
    void init();
    void recurseDirs();
    void onDirectoryModified(const Path &path);
    void onRecurseJobFinished(Map<Path, bool> &mPaths);
    void onParseJobFinished(GRParseJob *job, const Map<ByteArray, Map<Location, bool> > &entries);
    void remove(const Path &file, ScopedDB *grfiles = 0, ScopedDB *gr = 0);
    void dirty(uint32_t fileId, ScopedDB &db);
    void parse(const Path &path, unsigned flags);
    const Path &srcRoot() const { return mSrcRoot; }
private:
    const Path mSrcRoot;
    Map<Path, Map<ByteArray, time_t> > mFiles; // key: dir, value: fileName, last modified
    FileSystemWatcher *mWatcher;
    friend class FindFileJob;
};

#endif
