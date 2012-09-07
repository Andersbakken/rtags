#ifndef GRScanJob_h
#define GRScanJob_h

#include "ThreadPool.h"
#include "AbortInterface.h"
#include "Path.h"
#include "SignalSlot.h"

class GRScanJob : public ThreadPool::Job, public AbortInterface
{
public:
    GRScanJob(const Path &path);
    virtual void run();
    signalslot::Signal1<Map<Path, bool> &> &finished() { return mFinished; }

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
    signalslot::Signal1<Map<Path, bool> &> mFinished; // value => true means it's a source file
};

#endif
