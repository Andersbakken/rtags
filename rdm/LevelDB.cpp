#include "LevelDB.h"
#include <QReadWriteLock>

struct Locks {
    QReadWriteLock locks[Database::DatabaseTypeCount];
};
Q_GLOBAL_STATIC(Locks, readWriteLocks);

LevelDB::LevelDB()
    : mDB(0), mType(-1)
{
}

LevelDB::~LevelDB()
{
    if (mDB) {
        readWriteLocks()->locks[mType].unlock();
        delete mDB;
    }
}
bool LevelDB::open(Database::Type type, Mode mode)
{
    const QByteArray name = Database::databaseName(type);
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
        printf("[%s] %s:%d: failed to open %s\n", __func__, __FILE__, __LINE__, name.constData());
        Q_ASSERT(!mDB);
        lock.unlock();
        return false;
    }
    Q_ASSERT(mDB);
    mType = type;
    return true;
}
