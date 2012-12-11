#include "ScanJob.h"
#include "Server.h"
#include <fnmatch.h>

ScanJob::ScanJob(Mode mode, const Path &path, const shared_ptr<Project> &project)
    : mMode(mode), mPath(path), mFilters(Server::instance()->excludeFilter()), mProject(project)
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

ScanJob::FilterResult ScanJob::filter(const Path &path, const List<ByteArray> &filters)
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

Path::VisitResult ScanJob::visit(const Path &path, void *userData)
{
    ScanJob *recurseJob = reinterpret_cast<ScanJob*>(userData);
    const FilterResult result = filter(path, recurseJob->mFilters);
    switch (result) {
    case Filtered:
        return Path::Continue;
    case Directory:
        return Path::Recurse;
    case File:
        if (recurseJob->mMode == Sources)
            break;
        // fall through
    case Source:
        recurseJob->mPaths.insert(path);
        break;
    }
    return Path::Continue;
}
