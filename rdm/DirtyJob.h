#ifndef DirtyJob_h
#define DirtyJob_h

#include <QtCore>
#include "Indexer.h"
#include "Path.h"
#include "ThreadPool.h"

class DirtyJob : public QObject, public ThreadPool::Job
{
public:
    DirtyJob(Indexer *indexer, const Set<quint32> &dirty,
             const Hash<Path, List<ByteArray> > &toIndexPch,
             const Hash<Path, List<ByteArray> > &toIndex)
        : mIndexer(indexer), mDirty(dirty), mToIndexPch(toIndexPch), mToIndex(toIndex)
    {
    }

    virtual void run();
private:
    Indexer *mIndexer;
    const Set<quint32> mDirty;
    const Hash<Path, List<ByteArray> > mToIndexPch, mToIndex;
};

#endif
