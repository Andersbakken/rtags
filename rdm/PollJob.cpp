#include "PollJob.h"
#include "Indexer.h"

PollJob::PollJob(Indexer *idx, int i)
    : indexer(idx), id(i)
{
}

void PollJob::run()
{
    indexer->poll();
    emit complete(id, QList<QByteArray>());
}
