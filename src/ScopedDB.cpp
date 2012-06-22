#include "ScopedDB.h"
#include "Database.h"

ScopedDB::ScopedDB(Database *db, LockType lockType)
    : mData(new Data(db, lockType))
{
}

ScopedDB::Data::Data(Database *database, LockType lockType)
    : db(database), lock(lockType)
{
    if (db && lockType == Write) {
        db->lockForWrite();
    }
}

ScopedDB::Data::~Data()
{
    if (db && lock == Write)
        db->unlock();
}
