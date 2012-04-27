#include "TestJob.h"
#include "Server.h"
#include "Rdm.h"

TestJob::TestJob(const Path &p, int i)
    : Job(i), path(p)
{
}

void TestJob::execute()
{
    leveldb::DB *db = Server::instance()->db(Server::FileInformation);
    const bool found = Rdm::contains(db, path.constData());
    if (isAborted())
        return;
    write(found ? "1" : "0");
}
