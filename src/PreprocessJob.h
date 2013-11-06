#ifndef PreprocessorJob_h
#define PreprocessorJob_h

#include <rct/ThreadPool.h>
#include <rct/SignalSlot.h>
#include "CompileMessage.h"

class PreprocessJob : public ThreadPool::Job
{
public:
    PreprocessJob(String &&arguments, Path &&workingDirectory, List<String> &&projects);
    void exec()
    {
        mAsync = false;
        run();
    }
protected:
    virtual void run();
private:
    bool mAsync;
    const String mArguments;
    const Path mWorkingDirectory;
    List<String> mProjects;
};

#endif
