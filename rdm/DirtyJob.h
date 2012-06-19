#ifndef DirtyJob_h
#define DirtyJob_h

#include <QtCore>
#include "Indexer.h"
#include "Path.h"

class DirtyJob : public QObject, public QRunnable
{
public:
    DirtyJob(Indexer *indexer, const QSet<quint32> &dirty,
             const QHash<Path, QList<ByteArray> > &toIndexPch,
             const QHash<Path, QList<ByteArray> > &toIndex)
        : mIndexer(indexer), mDirty(dirty), mToIndexPch(toIndexPch), mToIndex(toIndex)
    {
    }

    virtual void run();
private:
    Indexer *mIndexer;
    const QSet<quint32> mDirty;
    const QHash<Path, QList<ByteArray> > mToIndexPch, mToIndex;
};

#endif
