#include "GRParser.h"
#include "GRJob.h"
#include "Server.h"

GRJob::GRJob(const Path &path)
    : mPath(path), mFilesBatch(0)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');
}

void GRJob::run()
{
    if (!mPath.isDir())
        return;
    ScopedDB filesDB = Server::instance()->db(Server::Files, Server::Erase, mPath);
    ScopedDB grDB = Server::instance()->db(Server::GRTags, Server::Write, mPath);
    {
        Batch filesBatch(filesDB);
        mFilesBatch = &filesBatch;
        Batch grBatch(grDB);
        mGRBatch = &grBatch;
        mPath.visit(&GRJob::visit, this);
    }
    finished()(mDirectories);
}

enum FilterResult {
    File,
    C,
    CPlusPlus,
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
        } else if (!strcmp(extension, "c")) {
            return C;
        } else if (path.isSource()) {
            return CPlusPlus;
        }
    }
    return File;
}

Path::VisitResult GRJob::visit(const Path &path, void *userData)
{
    const Path::Type type = path.type();
    const FilterResult result = filter(path, type, 10);
    GRJob *recurseJob = reinterpret_cast<GRJob*>(userData);
    switch (result) {
    case Filtered:
        return Path::Continue;
    case Directory:
        recurseJob->mDirectories.append(path);
        return Path::Recurse;
    case C:
        break;
    case CPlusPlus:
        break;
    case File:
        break;
    }
    const Path chopped = path.mid(recurseJob->mPath.size());
    recurseJob->mFilesBatch->add(chopped, true);
    return Path::Continue;
}
