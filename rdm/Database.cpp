#include "Database.h"
#include <leveldb/cache.h>

#ifdef USE_LEVELDB
Database::Database(const char *path, const Server::Options &options)
    : mDB(0)
{
    leveldb::Options opt;
    opt.create_if_missing = true;
    opt.block_cache = leveldb::NewLRUCache(options.cacheSizeMB * 1024 * 1024);
    leveldb::Status status = leveldb::DB::Open(opt, path, &mDB);
    if (!status.ok())
        mOpenError = status.ToString().c_str();
}

bool Database::isOpened() const
{
    return mDB;
}

void Database::close()
{
    delete mDB;
    mDB = 0;
    mOpenError.clear();
}

QByteArray Database::openError() const
{
    return mOpenError;
}

std::string Database::rawValue(const Slice &key, bool *ok) const
{
    std::string value;
    leveldb::Status status = mDB->Get(leveldb::ReadOptions(), key.mSlice, &value);
    if (ok)
        *ok = status.ok();
    return value;
}

int Database::setRawValue(const Slice &key, const Slice &value)
{
    mDB->Put(mWriteOptions, key.mSlice, value.mSlice);
    return value.size();
}
bool Database::contains(const Slice &key) const
{
    bool ok = false;
    rawValue(key, &ok);
    return ok;
}

void Database::remove(const Slice &key)
{
    mDB->Delete(mWriteOptions, key.mSlice);
}

Iterator *Database::createIterator() const
{
    return new Iterator(mDB->NewIterator(leveldb::ReadOptions()));
}

Iterator::Iterator(leveldb::Iterator *iterator)
    : mIterator(iterator)
{
}

Iterator::~Iterator()
{
    delete mIterator;
}
void Iterator::seekToFirst()
{
    mIterator->SeekToFirst();
}
void Iterator::seekToLast()

{    mIterator->SeekToLast();
}

void Iterator::seek(const Slice &slice)
{
    mIterator->Seek(slice.mSlice);
}

bool Iterator::isValid() const
{
    return mIterator->Valid();
}
void Iterator::next()
{
    mIterator->Next();
}
void Iterator::previous()
{
    mIterator->Prev();
}
Slice Iterator::key() const
{
    return Slice(mIterator->key());
}
Slice Iterator::value() const
{
    return mIterator->value();
}

Slice::Slice(const std::string &string)
    : mSlice(string.data(), string.size())
{}

Slice::Slice(const leveldb::Slice &slice)
    : mSlice(slice)
{}

Slice::Slice(const QByteArray &d)
    : mSlice(d.constData(), d.size())
{}

Slice::Slice(const char *d, int s)
    : mSlice(d, s == -1 ? strlen(d) : s)
{}

const char *Slice::data() const
{
    return mSlice.data();
}

int Slice::size() const
{
    return mSlice.size();
}

void Slice::clear()
{
    mSlice.clear();
}

Batch::Batch(Database *d)
    : mDB(d), mSize(0), mTotal(0)
{}

Batch::~Batch()
{
    flush();
}

int Batch::flush()
{
    const int was = mSize;
    if (mSize) {
        // error("About to write %d bytes to %p", batchSize, db);
        mDB->mDB->Write(mDB->mWriteOptions, &mBatch);
        mBatch.Clear();
        mTotal += mSize;
        // error("Wrote %d (%d) to %p", batchSize, totalWritten, db);
        mSize = 0;
    }
    return was;
}

int Batch::writeEncoded(const Slice &key, const Slice &data)
{
    mBatch.Put(key.mSlice, data.mSlice);
    mSize += data.size();
    if (mSize >= BatchThreshold) {
        flush();
    }
    return data.size();
}


void Batch::remove(const Slice &key)
{
    mBatch.Delete(key.mSlice);
}

#else
#error No Datatype selected
#endif
