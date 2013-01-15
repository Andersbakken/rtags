#include "CompileJob.h"
#include "Server.h"

CompileJob::CompileJob(const CompileMessage &message)
    : mArgs(message.arguments()), mPath(message.path())
{
}

void CompileJob::run()
{
    GccArguments args;
    if (!args.parse(mArgs, mPath))
        return;

    const Path srcRoot = args.projectRoot();
    if (!srcRoot.isEmpty())
        fileReady()(args, srcRoot);
}

