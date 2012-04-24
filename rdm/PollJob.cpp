#include "PollJob.h"
#include "Indexer.h"

PollJob::PollJob(Indexer *idx, int i)
    : Job(i), indexer(idx)
{
}

void PollJob::run()
{
    indexer->poll();
    emit complete(id(), QList<QByteArray>());
}
