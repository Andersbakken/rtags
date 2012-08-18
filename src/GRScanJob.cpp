#include "GRParser.h"
#include "GRScanJob.h"
#include "Server.h"

GRScanJob::GRScanJob(const Path &path)
    : mPath(path)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');

}

void GRScanJob::run()
{
    mPath.visit(&GRScanJob::visit, this);
    mFinished(mPaths);
}

enum FilterResult {
    File,
    Source,
    Directory,
    Filtered
};

static inline FilterResult filter(const Path &path, Path::Type type, int maxSymLinks)
{
    switch (type) {
    case Path::SymLink: {
        if (!--maxSymLinks)
            return Filtered;
        bool ok;
        const Path link = path.followLink(&ok);
        if (!ok)
            return Filtered;
        return filter(link, link.type(), maxSymLinks); }
    case Path::Directory: {
        const int lastSlash = path.lastIndexOf('/', path.size() - 2);
        if (lastSlash > 4 && !strncmp("/.git/", path.constData() + lastSlash - 5, 5))
            return Filtered;
        return Directory; }
    default:
        break;
    }
    const char *extension = path.extension();
    if (extension) {
        if (!strcmp(extension, "o")) {
            return Filtered;
        } else if (!strcmp(extension, "obj")) {
            return Filtered;
        } else if (!strcmp(extension, "lo")) {
            return Filtered;
        } else if (path.isSource() || path.isHeader()) {
            return Source;
        }
    }
    return File;
}

Path::VisitResult GRScanJob::visit(const Path &path, void *userData)
{
    const Path::Type type = path.type();
    const FilterResult result = filter(path, type, 10);
    GRScanJob *recurseJob = reinterpret_cast<GRScanJob*>(userData);
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
