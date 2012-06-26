#include "DirtyJob.h"
#include "Database.h"
#include "ScopedDB.h"
#include "IndexerJob.h"

void DirtyJob::run()
{
    Rdm::dirty(mDirty);
    if (mToIndexPch.isEmpty() && mToIndex.isEmpty()) {
        error() << "will assert" << mToIndex.size() << mToIndexPch.size() << mDirty;
    }
    assert(!mToIndexPch.isEmpty() || !mToIndex.isEmpty());
    for (Map<Path, List<ByteArray> >::const_iterator it = mToIndexPch.begin(); it != mToIndexPch.end(); ++it)
        mIndexer->index(it->first, it->second, IndexerJob::DirtyPch);
    for (Map<Path, List<ByteArray> >::const_iterator it = mToIndex.begin(); it != mToIndex.end(); ++it)
        mIndexer->index(it->first, it->second, IndexerJob::Dirty);
}
