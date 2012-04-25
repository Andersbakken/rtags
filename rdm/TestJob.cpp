#include "TestJob.h"
#include "Server.h"
#include "LevelDB.h"
#include "Rdm.h"

TestJob::TestJob(const Path &p, int i)
    : Job(i), path(p)
{
}

void TestJob::run()
{
#warning this is not good enough for system headers.
    LevelDB db;
    bool found = db.open(Server::Dependency, LevelDB::ReadOnly) && Rdm::contains(db.db(), path.constData());
    write(found ? "1" : "0");
    finish();
}
