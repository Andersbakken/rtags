#include "CompileJob.h"
#include "Server.h"

CompileJob::CompileJob(const CompileMessage &message)
    : mArgs(message.arguments()), mPath(message.path()), mProjects(message.projects())
{
}

CompileJob::CompileJob(const String& args, const String& path, const List<String>& projects)
    : mArgs(args), mPath(path), mProjects(projects)
{
}

void CompileJob::run()
{
    GccArguments args;
    if (args.parse(mArgs, mPath)) {
        argsReady()(args, mProjects);
    }
}

