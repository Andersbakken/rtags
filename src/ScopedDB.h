#ifndef ScopedDB_h
#define ScopedDB_h

#include <tr1/memory>
class Database;
class ScopedDB
{
public:
    enum LockType {
        Read,
        Write
    };
    ScopedDB(Database *db, LockType lockType);
    Database *operator->() { return mData->db; }
    operator Database *() { return mData->db; }
private:
    class Data
    {
    public:
        Data(Database *database, LockType lockType);
        ~Data();
        Database *db;
        LockType lock;
    };
    std::tr1::shared_ptr<Data> mData;
};

#endif
