#include "GRTags.h"
#include "RecurseJob.h"
#include "Server.h"

GRTags::GRTags(const Path &srcRoot)
    : mSrcRoot(srcRoot)
{
    recurseDirs();
}

void GRTags::recurseDirs()
{
    RecurseJob *job = new RecurseJob(mSrcRoot);
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
