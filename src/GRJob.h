#ifndef GRJob_h
#define GRJob_h

#include "ThreadPool.h"
#include "AbortInterface.h"
#include "Path.h"
#include "Database.h"
#include "signalslot.h"
#include "Indexer.h"

class GRJob : public ThreadPool::Job, public AbortInterface
{
public:
    GRJob(Indexer *indexer);
    virtual void run();
    signalslot::Signal1<const List<Path> &> &finished() { return mFinished; }
private:
    static Path::VisitResult visit(const Path &path, void *userData);
    Path mPath;
    Indexer *mIndexer;
    Batch *mFilesBatch, *mGRBatch;
    List<Path> mDirectories;
    ScopedDB *mDB;
    signalslot::Signal1<const List<Path> &> mFinished;
};

#endif
