#include "TestJob.h"
#include "Server.h"

TestJob::TestJob(const Path &p)
    : Job(0, shared_ptr<Project>()), path(p)
{
}

void TestJob::run()
{
    const bool found = Location::fileId(path);
    write(found ? "1" : "0");
}
