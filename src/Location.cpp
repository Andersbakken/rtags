#include "Location.h"
#include "Server.h"
#include "Database.h"

Map<Path, uint32_t> Location::sPathsToIds;
Map<uint32_t, Path> Location::sIdsToPaths;
uint32_t Location::sLastId = 0;
ReadWriteLock Location::sLock;

void Location::writeToDB(const Path &path, uint32_t id)
{
    // printf("Writing a value here %s %d\n", path.constData(), id);
    Server::instance()->db(Server::FileIds, ReadWriteLock::Write)->setValue<uint32_t>(path, id);
}
