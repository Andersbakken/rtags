#ifndef GRScanJob_h
#define GRScanJob_h

#include "ThreadPool.h"
#include "AbortInterface.h"
#include "Path.h"
#include "SignalSlot.h"
#include "Project.h"

class GRScanJob : public ThreadPool::Job, public AbortInterface
{
public:
    GRScanJob(const Path &path, const shared_ptr<Project> &project);
    virtual void run();
    signalslot::Signal1<const Map<Path, bool> &> &finished() { return mFinished; }

    enum FilterResult {
        Filtered,
        File,
        Source,
        Directory
    };

    static FilterResult filter(const Path &path, const List<ByteArray> &filters);
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    const List<ByteArray> &mFilters;
    Map<Path, bool> mPaths;
    signalslot::Signal1<const Map<Path, bool> &> mFinished; // value => true means it's a source file

    weak_ptr<Project> mProject;
};

#endif
