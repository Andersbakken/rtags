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

Path::VisitResult RecurseJob::visit(const Path &path, void *userData)
{
    if (path.isFile()) {
        RecurseJob *recurseJob = reinterpret_cast<RecurseJob*>(userData);
        const Path chopped = path.mid(recurseJob->mPath.size());
        recurseJob->mBatch->add(chopped, true);
        // recurseJob->mPaths.append(chopped);
        return Path::Continue;
    }
    return Path::Recurse;
}
