#ifndef DirtyJob_h
#define DirtyJob_h

#include <QtCore>
#include <Path.h>

class DirtyJob : public QObject, public QRunnable
{
public:
    DirtyJob(const QSet<Path> &dirty,
             const QHash<Path, QList<QByteArray> > &toIndexPch,
             const QHash<Path, QList<QByteArray> > &toIndex)
        : mDirty(dirty), mToIndexPch(toIndexPch), mToIndex(toIndex)
    {}

    virtual void run()
    {
        dirty();
        Indexer *indexer = Indexer::instance();
        for (QHash<Path, QList<QByteArray> >::const_iterator it = mToIndexPch.begin(); it != mToIndexPch.end(); ++it)
            indexer->index(it.key(), it.value());
        for (QHash<Path, QList<QByteArray> >::const_iterator it = mToIndex.begin(); it != mToIndex.end(); ++it)
            indexer->index(it.key(), it.value());
    }
    void dirty();
private:
    const QSet<Path> mDirty;
    const QHash<Path, QList<QByteArray> > mToIndexPch, mToIndex;
};

#endif
