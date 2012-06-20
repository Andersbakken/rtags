#ifndef DirtyJob_h
#define DirtyJob_h

#include <QtCore>
#include "Indexer.h"
#include "Path.h"

class DirtyJob : public QObject, public QRunnable
{
public:
    DirtyJob(Indexer *indexer, const Set<quint32> &dirty,
             const Hash<Path, QList<ByteArray> > &toIndexPch,
             const Hash<Path, QList<ByteArray> > &toIndex)
        : mIndexer(indexer), mDirty(dirty), mToIndexPch(toIndexPch), mToIndex(toIndex)
    {
    }

    virtual void run();
private:
    Indexer *mIndexer;
    const Set<quint32> mDirty;
    const Hash<Path, QList<ByteArray> > mToIndexPch, mToIndex;
};

#endif
