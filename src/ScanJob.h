#ifndef ScanJob_h
#define ScanJob_h

#include "ThreadPool.h"
#include "Path.h"
#include "SignalSlot.h"
#include "Project.h"

class ScanJob : public ThreadPool::Job
{
public:
    ScanJob(const Path &path, const shared_ptr<Project> &project);
    virtual void run();
    signalslot::Signal1<const Set<Path> &>&finished() { return mFinished; }
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    const List<ByteArray> &mFilters;
    Set<Path> mPaths;
    signalslot::Signal1<const Set<Path> &> mFinished; // value => true means it's a source file

    weak_ptr<Project> mProject;
};

#endif
