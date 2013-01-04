#ifndef AutoProjectJob_h
#define AutoProjectJob_h

#include "ThreadPool.h"
#include "Path.h"
#include "ProjectMessage.h"
#include "SignalSlot.h"
#include "GccArguments.h"

class AutoProjectJob : public ThreadPool::Job
{
public:
    AutoProjectJob(const ProjectMessage &message);
    virtual void run();
    signalslot::Signal2<GccArguments, Path> &fileReady() { return mFileReady; }
private:
    const ByteArray mArgs;
    const Path mPath;
    signalslot::Signal2<GccArguments, Path> mFileReady;

};

#endif
