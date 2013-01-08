#include "ScanJob.h"
#include "Server.h"
#include "Filter.h"

ScanJob::ScanJob(const Path &path, const shared_ptr<Project> &project)
    : mPath(path), mFilters(Server::instance()->excludeFilters()), mProject(project)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');
}

void ScanJob::run()
{
    mPath.visit(&ScanJob::visit, this);
    if (shared_ptr<Project> project = mProject.lock())
        mFinished(mPaths);
}

Path::VisitResult ScanJob::visit(const Path &path, void *userData)
{
    ScanJob *recurseJob = reinterpret_cast<ScanJob*>(userData);
    const Filter::Result result = Filter::filter(path, recurseJob->mFilters);
    switch (result) {
    case Filter::Filtered:
        return Path::Continue;
    case Filter::Directory:
        return Path::Recurse;
    case Filter::File:
    case Filter::Source:
        recurseJob->mPaths.insert(path);
        break;
    }
    return Path::Continue;
}
