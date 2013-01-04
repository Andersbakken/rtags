#include "AutoProjectJob.h"
#include "Server.h"

AutoProjectJob::AutoProjectJob(const ProjectMessage &message)
    : mArgs(message.arguments().first()), mPath(message.path())
{
}

void AutoProjectJob::run()
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

