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
    const String mArgs;
    const Path mPath;
    signalslot::Signal1<GccArguments> mArgsReady;

};

#endif
