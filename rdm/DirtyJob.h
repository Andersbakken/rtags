#ifndef DirtyJob_h
#define DirtyJob_h

#include <QtCore>
#include "Indexer.h"
#include "Path.h"

class DirtyJob : public QObject, public QRunnable
{
public:
    DirtyJob(Indexer *indexer, const QSet<Path> &dirty,
             const QHash<Path, QList<QByteArray> > &toIndexPch,
             const QHash<Path, QList<QByteArray> > &toIndex)
        : mIndexer(indexer), mDirty(dirty), mToIndexPch(toIndexPch), mToIndex(toIndex)
    {}

    virtual void run()
    {
        dirty();
        for (QHash<Path, QList<QByteArray> >::const_iterator it = mToIndexPch.begin(); it != mToIndexPch.end(); ++it)
            mIndexer->index(it.key(), it.value());
        for (QHash<Path, QList<QByteArray> >::const_iterator it = mToIndex.begin(); it != mToIndex.end(); ++it)
            mIndexer->index(it.key(), it.value());
    }
    void dirty();
private:
    Indexer *mIndexer;
    const QSet<Path> mDirty;
    const QHash<Path, QList<QByteArray> > mToIndexPch, mToIndex;
};

#endif
