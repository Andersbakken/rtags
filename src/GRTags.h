#ifndef GRTags_h
#define GRTags_h

#include "Path.h"
#include "List.h"
#include "FileSystemWatcher.h"

class Indexer;
class GRTags
{
public:
    GRTags(Indexer *indexer);
    void recurseDirs();
    void onRecurseJobFinished(const List<Path> &mPaths);
private:
    Indexer *mIndexer;
    FileSystemWatcher *mWatcher;
};

#endif
