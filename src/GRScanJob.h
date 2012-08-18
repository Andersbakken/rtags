#ifndef GRScanJob_h
#define GRScanJob_h

#include "ThreadPool.h"
#include "AbortInterface.h"
#include "Path.h"
#include "Database.h"
#include "signalslot.h"
#include "ScopedDB.h"

class GRScanJob : public ThreadPool::Job, public AbortInterface
{
public:
    GRScanJob(const Path &path);
    virtual void run();
    signalslot::Signal1<Map<Path, bool> &> &finished() { return mFinished; }
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    Map<Path, bool> mPaths;
    signalslot::Signal1<Map<Path, bool> &> mFinished; // value => true means it's a source file
};

#endif
