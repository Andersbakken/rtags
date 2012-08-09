#ifndef ScopedDB_h
#define ScopedDB_h

#include <tr1/memory>
#include "ReadWriteLock.h"
class Database;
class ScopedDB
{
public:
    ScopedDB();
    ScopedDB(Database *db, ReadWriteLock::LockType lockType, bool clear);
    Database *operator->() { return mData->db; }
    operator Database *() { return mData->db; }

    Database *database() const { return mData->db; }
    ReadWriteLock::LockType lockType() const { return mData->lockType; }

    void reset() { mData.reset(); }
private:
    class Data
    {
    public:
        Data(Database *database, ReadWriteLock::LockType lockType);
        ~Data();
        Database *db;
        const ReadWriteLock::LockType lockType;
    };
    std::tr1::shared_ptr<Data> mData;
};

#endif
