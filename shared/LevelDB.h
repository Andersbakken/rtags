#ifndef LevelDB_h
#define LevelDB_h

#include "Database.h"
#include <leveldb/db.h>

class LevelDBConnection : public Connection
{
public:
    LevelDBConnection(leveldb::DB *db, char prefix)
        : mDB(db), mPrefix(prefix)
    {}

    virtual QByteArray readData(const QByteArray &k) const
    {
        std::string val;
        const QByteArray key = mPrefix + k;
        mDB->Get(leveldb::ReadOptions(), leveldb::Slice(key.constData(), key.size()), &val);
        return QByteArray(val.c_str(), val.size());
    }
    virtual void writeData(const QByteArray &key, const QByteArray &value)
    {
        mDB->Put(leveldb::WriteOptions(),
                 leveldb::Slice((mPrefix + key).constData(), key.size() + 1),
                 leveldb::Slice(value.constData(), value.size()));
    }
private:
    leveldb::DB *mDB;
    const char mPrefix;
};

class LevelDBIterator : public Database::iterator
{
public:
    LevelDBIterator(leveldb::DB *db)
        : mDB(db), mIterator(db->NewIterator(leveldb::ReadOptions()))
    {
        mIterator->SeekToFirst();
    }
    virtual ~LevelDBIterator()
    {
        delete mIterator;
    }
    virtual QByteArray key() const
    {
        const leveldb::Slice k = mIterator->key();
        return QByteArray(k.data(), k.size());
    }
    virtual QByteArray value() const
    {
        const leveldb::Slice v = mIterator->value();
        return QByteArray(v.data(), v.size());
    }
    virtual bool seek(const QByteArray &key)
    {
        mIterator->Seek(leveldb::Slice(key.constData(), key.size()));
        return mIterator->Valid();
    }
    virtual bool next()
    {
        mIterator->Next();
        return mIterator->Valid();
    }
private:
    leveldb::DB *mDB;
    leveldb::Iterator *mIterator;
};

class LevelDB : public Database
{
public:
    LevelDB()
        : mDB(0)
    {}
    virtual ~LevelDB()
    {
        Q_ASSERT(!mDB);
    }

    virtual bool isOpened() const
    {
        return mDB;
    }
    virtual void closeDatabase()
    {
        delete mDB;
        mDB = 0;
    }

    virtual iterator *createIterator() const
    {
        return new LevelDBIterator(mDB);
    }

    virtual Connection *createConnection(ConnectionType type)
    {
        return new LevelDBConnection(mDB, char('a' + type));
    }

    virtual bool openDatabase(const Path &path, Mode mode)
    {
        Q_ASSERT(!mDB);
        leveldb::Options dbOptions;
        if (mode != ReadOnly)
            dbOptions.create_if_missing = true;
        const leveldb::Status status = leveldb::DB::Open(dbOptions, path.constData(), &mDB);
        if (!status.ok()) {
            fprintf(stderr, "Unable to open db [%s]: %s\n", path.constData(), status.ToString().c_str());
            return false;
        }
        return true;
    }

private:
    leveldb::DB *mDB;
};

#endif
