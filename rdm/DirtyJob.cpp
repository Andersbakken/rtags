#include "DirtyJob.h"
#include "Database.h"
#include "Server.h"
#include "IndexerJob.h"

void DirtyJob::run()
{
    Rdm::dirty(mDirty);
    if (mToIndexPch.isEmpty() && mToIndex.isEmpty()) {
        error() << "will assert" << mToIndex.size() << mToIndexPch.size() << mDirty;
    }
    Q_ASSERT(!mToIndexPch.isEmpty() || !mToIndex.isEmpty());
    for (QHash<Path, QList<ByteArray> >::const_iterator it = mToIndexPch.begin(); it != mToIndexPch.end(); ++it)
        mIndexer->index(it.key(), it.value(), IndexerJob::DirtyPch);
    for (QHash<Path, QList<ByteArray> >::const_iterator it = mToIndex.begin(); it != mToIndex.end(); ++it)
        mIndexer->index(it.key(), it.value(), IndexerJob::Dirty);
}
