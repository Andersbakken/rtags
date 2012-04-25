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
    LevelDB db;
    bool found = db.open(Server::Dependency, LevelDB::ReadOnly) && Rdm::contains(db.db(), path.constData());
    write(found ? "1" : "0");
    finish();
}
