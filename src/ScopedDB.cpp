#include "ScopedDB.h"
#include "Database.h"

ScopedDB::ScopedDB(Database *db, ReadWriteLock::LockType lockType, bool clear)
    : mData(new Data(db, lockType))
{
    if (clear) {
        assert(db);
        assert(lockType == ReadWriteLock::Write);
        db->clear();
    }
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
