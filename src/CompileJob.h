#ifndef CompileJob_h
#define CompileJob_h

#include <rct/ThreadPool.h>
#include <rct/Path.h>
#include "CompileMessage.h"
#include <rct/SignalSlot.h>
#include "GccArguments.h"

class CompileJob : public ThreadPool::Job
{
public:
    CompileJob(const CompileMessage &message);
    CompileJob(const String& args, const String& path, const List<String>& projects);
    virtual void run();
    signalslot::Signal2<const GccArguments&, const List<String> &> &argsReady() { return mArgsReady; }
private:
    const String mArgs;
    const Path mPath;
    const List<String> mProjects;
    signalslot::Signal2<const GccArguments &, const List<String> &> mArgsReady;

};

#endif
