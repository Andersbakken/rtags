#include "TestJob.h"
#include "Server.h"
#include "ScopedDB.h"

TestJob::TestJob(const Path &p, int i)
    : Job(0, 0), path(p)
{
    setId(i);
}

void TestJob::execute()
{
    const bool found = Location::fileId(path);
    write(found ? "1" : "0");
}
