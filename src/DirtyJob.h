#ifndef DirtyJob_h
#define DirtyJob_h

#include "Indexer.h"
#include "Path.h"
#include "ThreadPool.h"

class DirtyJob : public QObject, public ThreadPool::Job
{
public:
    DirtyJob(Indexer *indexer, const Set<uint32_t> &dirty,
             const Map<Path, List<ByteArray> > &toIndexPch,
             const Map<Path, List<ByteArray> > &toIndex)
        : mIndexer(indexer), mDirty(dirty), mToIndexPch(toIndexPch), mToIndex(toIndex)
    {
    }

    virtual void run();
private:
    Indexer *mIndexer;
    const Set<uint32_t> mDirty;
    const Map<Path, List<ByteArray> > mToIndexPch, mToIndex;
};

#endif
