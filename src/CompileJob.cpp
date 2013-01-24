#include "CompileJob.h"
#include "CompilerManager.h"
#include "Server.h"

CompileJob::CompileJob(const CompileMessage &message)
    : mArgs(message.arguments()), mPath(message.path())
{
}

void CompileJob::run()
{
    GccArguments args;
    if (args.parse(mArgs, mPath)) {
        args.addFlags(CompilerManager::flags(args.compiler()));
        argsReady()(args);
    }
}

