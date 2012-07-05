#include "ScopedDB.h"
#include "Database.h"

ScopedDB::ScopedDB(Database *db, ReadWriteLock::LockType lockType)
    : mData(new Data(db, lockType))
{
}

ScopedDB::ScopedDB()
    : mData(new Data(0, ReadWriteLock::Read))
{
}

ScopedDB::Data::Data(Database *database, ReadWriteLock::LockType type)
    : db(database), lockType(type)
{
    if (db) {
        db->lock(lockType);
    }
}

ScopedDB::Data::~Data()
{
    if (db)
        db->unlock();
}
