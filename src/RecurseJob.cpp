#include "RecurseJob.h"
#include "Server.h"

RecurseJob::RecurseJob(const Path &path)
    : mPath(path), mBatch(0)
{
    if (!mPath.endsWith('/'))
        mPath.append('/');
}

void RecurseJob::run()
{
    if (!mPath.isDir())
        return;
    ScopedDB db = Server::instance()->db(Server::Files, Server::Erase, mPath);
    {
        Batch batch(db);
        mBatch = &batch;
        mPath.visit(&RecurseJob::visit, this);
    }
}

static inline bool filter(const Path &path, Path::Type type, int maxSymLinks, bool &isDir)
{
    switch (type) {
    case Path::SymLink: {
        if (!--maxSymLinks)
            return false;
        bool ok;
        const Path link = path.followLink(&ok);
        return ok && filter(link, link.type(), maxSymLinks, isDir); }
    case Path::Directory: {
        isDir = true;
        const int lastSlash = path.lastIndexOf('/', path.size() - 2);
        if (lastSlash > 4 && !strncmp("/.git/", path.constData() + lastSlash - 5, 5))
            return false;
        return true; }
    default:
        break;
    }
    isDir = false;
    const char *extension = path.extension();
    return !extension || strcmp(extension, "o");
}

Path::VisitResult RecurseJob::visit(const Path &path, void *userData)
{
    const Path::Type type = path.type();
    bool dir = false;
    if (!filter(path, type, 10, dir))
        return Path::Continue;
    if (dir)
        return Path::Recurse;
    RecurseJob *recurseJob = reinterpret_cast<RecurseJob*>(userData);
    const Path chopped = path.mid(recurseJob->mPath.size());
    recurseJob->mBatch->add(chopped, true);
    return Path::Continue;
}
