#include "RecurseJob.h"
#include "Server.h"

RecurseJob::RecurseJob(const Path &path)
    : mPath(path), mBatch(0)
{

}

void RecurseJob::run()
{
    ScopedDB db = Server::instance()->db(Server::Files, ReadWriteLock::Write, mPath);
    {
        Batch batch(db);
        mBatch = &batch;
        mPath.visit(&RecurseJob::visit, this);
    }
}

Path::VisitResult RecurseJob::visit(const Path &path, void *userData)
{
    if (path.isFile()) {
        RecurseJob *that = reinterpret_cast<RecurseJob*>(userData);
        that->mBatch->add(path, true);
        that->mPaths.append(path);
        return Path::Continue;
    }
    return Path::Recurse;
}
