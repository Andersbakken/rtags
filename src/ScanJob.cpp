#include "ScanJob.h"
#include "Server.h"
#include "Filter.h"
#include "Project.h"

ScanJob::ScanJob(const Path &path)
    : mPath(path), mFilters(Server::instance()->options().excludeFilters)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');
}

void ScanJob::run()
{
    mPath.visit(&ScanJob::visit, this);
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
        if (Path::exists(path + "/.rtags-ignore"))
            return Path::Continue;
        return Path::Recurse;
    case Filter::File:
    case Filter::Source:
        recurseJob->mPaths.insert(path);
        break;
    }
    return Path::Continue;
}
