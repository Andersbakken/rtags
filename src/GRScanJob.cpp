#include "GRParser.h"
#include "GRScanJob.h"
#include "Server.h"
#include <fnmatch.h>

GRScanJob::GRScanJob(const Path &path, const shared_ptr<Project> &project)
    : mPath(path), mFilters(Server::instance()->excludeFilter()), mProject(project)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');
}

void GRScanJob::run()
{
    mPath.visit(&GRScanJob::visit, this);
    if (shared_ptr<Project> project = mProject.lock())
        mFinished(mPaths);
}

GRScanJob::FilterResult GRScanJob::filter(const Path &path, const List<ByteArray> &filters)
{
    const int size = filters.size();
    for (int i=0; i<size; ++i) {
        const ByteArray &filter = filters.at(i);
        if (!fnmatch(filter.constData(), path.constData(), 0))
            return Filtered;
    }

    if (path.isDir()) {
        return Directory;
    }
    const char *ext = path.extension();
    if (ext && (Path::isSource(ext) || Path::isHeader(ext)))
        return Source;
    return File;
}

Path::VisitResult GRScanJob::visit(const Path &path, void *userData)
{
    GRScanJob *recurseJob = reinterpret_cast<GRScanJob*>(userData);
    const FilterResult result = filter(path, recurseJob->mFilters);
    switch (result) {
    case Filtered:
        return Path::Continue;
    case Directory:
        return Path::Recurse;
    case Source:
        recurseJob->mPaths[path] = true;
        break;
    case File:
        recurseJob->mPaths[path] = false;
        break;
    }
    return Path::Continue;
}
