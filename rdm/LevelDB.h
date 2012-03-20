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

    leveldb::DB *mDB;
};

class LevelDBIterator : public Database::iterator
{
public:
    LevelDBIterator(Database::ConnectionType t, leveldb::DB *db)
        : Database::iterator(t), mDB(db), mIterator(db->NewIterator(leveldb::ReadOptions()))
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
        return isValid();
    }

    virtual bool isValid() const
    {
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
    {
        memset(mDatabases, 0, NumConnectionTypes * sizeof(leveldb::DB*));
    }
    virtual ~LevelDB()
    {
    }

    virtual bool isOpened() const
    {
        return mDatabases[0];
    }
    virtual void closeDatabase()
    {
        for (int i=0; i<NumConnectionTypes; ++i) {
            delete mDatabases[i];
            mDatabases[i] = 0;
        }
    }

    virtual iterator *createIterator(ConnectionType type) const
    {
        return new LevelDBIterator(type, mDatabases[type]);
    }

    virtual Connection *createConnection(ConnectionType type) const
    {
        return new LevelDBConnection(mDatabases[type]);
    }

    virtual bool openDatabase(const Path &path, Mode mode)
    {
        if (!mkdir(path.constData(), S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) || errno == EEXIST) {
            leveldb::Options dbOptions;
            if (mode != ReadOnly)
                dbOptions.create_if_missing = true;
            for (int i=0; i<NumConnectionTypes; ++i) {
                Path p = path;
                switch (i) {
                case General: p += "/general"; break;
                case Dictionary: p += "/dictionary"; break;
                case References: p += "/references"; break;
                case Targets: p += "/targets"; break;
                case ExtraDeclarations: p += "/extradeclarations"; break;
                case Super: p += "/super"; break;
                case Subs: p += "/subs"; break;
                }
                Q_ASSERT(p != path);
                const leveldb::Status status = leveldb::DB::Open(dbOptions, p.constData(), &mDatabases[i]);
                if (!status.ok()) {
                    fprintf(stderr, "Unable to open db [%s]: %s\n", p.constData(), status.ToString().c_str());
                    closeDatabase();
                    break;
                }
            }
        }
        return isOpened();
    }
private:
    leveldb::DB *mDatabases[NumConnectionTypes];
};

#endif
