#ifndef CompileJob_h
#define CompileJob_h

#include "ThreadPool.h"
#include "Path.h"
#include "CompileMessage.h"
#include "SignalSlot.h"
#include "GccArguments.h"

class CompileJob : public ThreadPool::Job
{
public:
    CompileJob(const CompileMessage &message);
    virtual void run();
    signalslot::Signal1<GccArguments> &argsReady() { return mArgsReady; }
private:
    const ByteArray mArgs;
    const Path mPath;
    const Path mCpp;
    signalslot::Signal1<GccArguments> mArgsReady;

};

#endif
