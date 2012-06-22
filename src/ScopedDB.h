#ifndef ScopedDB_h
#define ScopedDB_h

class Database;
#include <QSharedDataPointer>
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
    class Data : public QSharedData
    {
    public:
        Data(Database *database, LockType lockType);
        ~Data();
        Database *db;
        LockType lock;
    };
    QSharedDataPointer<Data> mData;
};

#endif
