#ifndef ScanJob_h
#define ScanJob_h

#include <rct/ThreadPool.h>
#include <rct/Path.h>
#include <rct/SignalSlot.h>
class Project;
class ScanJob : public ThreadPool::Job
{
public:
    ScanJob(const Path &path);
    virtual void run();
    signalslot::Signal1<Set<Path> > &finished() { return mFinished; }
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    const List<String> &mFilters;
    Set<Path> mPaths;
    signalslot::Signal1<Set<Path> > mFinished;
};

#endif
