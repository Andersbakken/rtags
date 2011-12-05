#ifndef LevelDB_h
#define LevelDB_h

#include "Database.h"
#include <leveldb/db.h>
#include <errno.h>

class LevelDBConnection : public Connection
{
public:
    LevelDBConnection(leveldb::DB *db)
        : mDB(db)
    {}

    virtual QByteArray readData(const QByteArray &key) const
    {
        std::string val;
        mDB->Get(leveldb::ReadOptions(), leveldb::Slice(key.constData(), key.size()), &val);
        return QByteArray(val.c_str(), val.size());
    }
    virtual void writeData(const QByteArray &key, const QByteArray &value)
    {
        if (value.isEmpty()) {
            mDB->Delete(leveldb::WriteOptions(),
                        leveldb::Slice(key.constData(), key.size()));
        } else {
            mDB->Put(leveldb::WriteOptions(),
                     leveldb::Slice(key.constData(), key.size()),
                     leveldb::Slice(value.constData(), value.size()));
        }
    }
private:
    leveldb::DB *mDB;
};

class LevelDBIterator : public Database::iterator
{
public:
    LevelDBIterator(Database::ConnectionType t, leveldb::DB *db)
        : Database::iterator(t), mDB(db), mIterator(db->NewIterator(leveldb::ReadOptions()))
    {
        if (type == Database::All) {
            mIterator->SeekToFirst();
        } else {
            char ch = 'a';
            ch += type;
            mIterator->Seek(leveldb::Slice(&ch, 1));
        }
    }
    virtual Database::ConnectionType currentType() const
    {
        if (type == Database::All) {
            const QByteArray k = key();
            if (!k.isEmpty()) {
                return static_cast<Database::ConnectionType>(k.at(0) - 'a');
            }
        }
        return type;
    }

    virtual ~LevelDBIterator()
    {
        delete mIterator;
    }
    virtual QByteArray key() const
    {
        const leveldb::Slice k = mIterator->key();
        return QByteArray(k.data() + 1, k.size() - 1);
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
        return isValid();
    }

    virtual bool isValid() const
    {
        if (mIterator->Valid()) {
            if (type != Database::All) {
                const leveldb::Slice k = mIterator->key();
                if (!k.size() || k.data()[0] != ('a' + type))
                    return false;
            }
            return true;
        }
        return false;
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

    virtual iterator *createIterator(ConnectionType type) const
    {
        return new LevelDBIterator(type, mDB);
    }

    virtual Connection *createConnection(ConnectionType type)
    {
        Path p = path();
        switch (type) {
        case General: p += "/general"; break;
        case Dictionary: p += "/dictionary"; break;
        case References: p += "/references"; break;
        case Targets: p += "/targets"; break;
        default: Q_ASSERT(0); break;
        }
        leveldb::Options dbOptions;
        if (mode() != ReadOnly)
            dbOptions.create_if_missing = true;
        const leveldb::Status status = leveldb::DB::Open(dbOptions, p.constData(), &mDB);
        if (!status.ok()) {
            fprintf(stderr, "Unable to open db [%s]: %s\n", p.constData(), status.ToString().c_str());
            return 0;
        }
        return new LevelDBConnection(mDB);
    }

    virtual bool openDatabase(const Path &path, Mode)
    {
        return !mkdir(path.constData(), S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) || errno == EEXIST;
    }

private:
    leveldb::DB *mDB;
};

#endif
