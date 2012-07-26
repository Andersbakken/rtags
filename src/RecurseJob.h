#ifndef RecurseJob_h
#define RecurseJob_h

#include "ThreadPool.h"
#include "AbortInterface.h"
#include "Path.h"
#include "Database.h"
#include "signalslot.h"

class RecurseJob : public ThreadPool::Job, public AbortInterface
{
public:
    RecurseJob(const Path &path);
    virtual void run();
    // signalslot::Signal1<const List<Path> &> &finished() { return mFinished; }
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    // List<Path> mPaths;
    Batch *mBatch;
    // signalslot::Signal1<const List<Path> &> mFinished;
};

#endif
