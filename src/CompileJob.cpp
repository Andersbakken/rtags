#include "CompileJob.h"
#include "Server.h"

CompileJob::CompileJob(const ProjectMessage &message)
    : mArgs(message.arguments()), mPath(message.path())
{
}

void CompileJob::run()
{
    GccArguments args;
    if (!args.parse(mArgs, mPath))
        return;

    Path srcRoot = Server::findProjectRoot(args.unresolvedInputFiles().first());
    if (srcRoot.isEmpty())
        srcRoot = Server::findProjectRoot(args.inputFiles().first());
    if (!srcRoot.isEmpty())
        fileReady()(args, srcRoot);
}

