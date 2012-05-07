#include "TestJob.h"
#include "Server.h"
#include "Rdm.h"

TestJob::TestJob(const Path &p, int i)
    : Job(i), path(p)
{
}

void TestJob::execute()
{
    Database *db = Server::instance()->db(Server::FileInformation);
    const bool found = db->contains(path);
    if (isAborted())
        return;
    write(found ? "1" : "0");
}
