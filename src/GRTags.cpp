#include "GRTags.h"
#include "GRJob.h"
#include "Server.h"
#include "Indexer.h"

GRTags::GRTags(Indexer *indexer)
    : mIndexer(indexer)
{
}

void GRTags::recurseDirs()
{
    GRJob *job = new GRJob(mIndexer);
    job->finished().connect(this, &GRTags::onRecurseJobFinished);
    Server::instance()->threadPool()->start(job);
}

void GRTags::onRecurseJobFinished(const List<Path> &mPaths)
{
    // ### need to watch these directories for changes, probably only care when
    // ### files are added or removed so FileSystemWatcher needs to be beefed up
// #warning not done
    // error() << mPaths;
}
