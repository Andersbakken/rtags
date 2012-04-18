#include "LevelDB.h"
#include <QReadWriteLock>

struct Locks {
    QReadWriteLock locks[Server::DatabaseTypeCount];
};
Q_GLOBAL_STATIC(Locks, readWriteLocks);

LevelDB::LevelDB()
    : mDB(0), mType(-1)
{
}

LevelDB::~LevelDB()
{
    close();
}

void LevelDB::close()
{
    if (mDB) {
        readWriteLocks()->locks[mType].unlock();
        delete mDB;
        mDB = 0;
        mType = -1;
    }
}
bool LevelDB::open(Server::DatabaseType type, Mode mode, QByteArray *error)
{
    const QByteArray name = Server::databaseName(type);
    Q_ASSERT(!name.isEmpty());
    leveldb::Options options;
    if (mode == ReadWrite)
        options.create_if_missing = true;
    QReadWriteLock &lock = readWriteLocks()->locks[type];
    switch (mode) {
    case ReadWrite:
        lock.lockForWrite();
        break;
    case ReadOnly:
        lock.lockForRead();
        break;
    }
    const leveldb::Status status = leveldb::DB::Open(options, name.constData(), &mDB);
    if (!status.ok()) {
        if (error)
            *error = status.ToString().c_str();
        Q_ASSERT(!mDB);
        lock.unlock();
        return false;
    }
    Q_ASSERT(mDB);
    mType = type;
    return true;
}
