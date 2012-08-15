#include "TestJob.h"
#include "Server.h"
#include "ScopedDB.h"

TestJob::TestJob(const Path &p)
    : Job(0, shared_ptr<Project>()), path(p)
{
}

void TestJob::execute()
{
    const bool found = Location::fileId(path);
    write(found ? "1" : "0");
}
