#include "CompileJob.h"
#include "CompilerManager.h"
#include "Server.h"

CompileJob::CompileJob(const CompileMessage &message)
    : mArgs(message.arguments()), mPath(message.path()), mCpp(message.cpp())
{
}

void CompileJob::run()
{
    GccArguments args;
    if (args.parse(mArgs, mPath)) {
        args.addFlags(CompilerManager::flags(args.compiler(), mCpp));
        argsReady()(args);
    }
}

