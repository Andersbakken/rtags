#ifndef CompileJob_h
#define CompileJob_h

#include "ThreadPool.h"
#include "Path.h"
#include "ProjectMessage.h"
#include "SignalSlot.h"
#include "GccArguments.h"

class CompileJob : public ThreadPool::Job
{
public:
    CompileJob(const ProjectMessage &message);
    virtual void run();
    signalslot::Signal2<GccArguments, Path> &fileReady() { return mFileReady; }
private:
    const ByteArray mArgs;
    const Path mPath;
    signalslot::Signal2<GccArguments, Path> mFileReady;

};

#endif
