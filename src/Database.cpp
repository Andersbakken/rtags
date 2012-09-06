#include "Database.h"
#include <leveldb/cache.h>
#include <leveldb/comparator.h>
#include "RTags.h"

// ================== Slice ==================

Slice::Slice(const std::string &string)
    : mSlice(string.data(), string.size())
{}

Slice::Slice(const leveldb::Slice &slice)
    : mSlice(slice)
{}

Slice::Slice(const ByteArray &d)
    : mSlice(d.constData(), d.size())
{}

Slice::Slice(const char *d, int s)
    : mSlice(d, d ? (s == -1 ? strlen(d) : s) : 0)
{}

bool Slice::operator==(const Slice &other) const
{
    return mSlice == other.mSlice;
}

bool Slice::operator!=(const Slice &other) const
{
    return mSlice != other.mSlice;
}

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

// ================== Iterator ==================

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

Slice Iterator::rawValue() const
{
    return mIterator->value();
}

// ================== Database ==================

class LocationComparator : public leveldb::Comparator
{
public:
    int Compare(const leveldb::Slice &left, const leveldb::Slice &right) const
    {
        assert(left.size() == right.size());
        assert(left.size() == 8);
        const uint32_t *l = reinterpret_cast<const uint32_t*>(left.data());
        const uint32_t *r = reinterpret_cast<const uint32_t*>(right.data());
        if (*l < *r)
            return -1;
        if (*l > *r)
            return 1;
        ++l;
        ++r;
        if (*l < *r)
            return -1;
        if (*l > *r)
            return 1;
        return 0;
    }

    const char* Name() const { return "LocationComparator"; }
    void FindShortestSeparator(std::string*, const leveldb::Slice&) const { }
    void FindShortSuccessor(std::string*) const { }
};

Database::Database(const Path &path, int cacheSizeMB, unsigned flags)
    : mDB(0), mLocationComparator(flags & LocationKeys ? new LocationComparator : 0), mFlags(flags)
{
    mOptions.create_if_missing = true;
    if (flags & LocationKeys)
        mOptions.comparator = mLocationComparator;
    if (cacheSizeMB)
        mOptions.block_cache = leveldb::NewLRUCache(cacheSizeMB * 1024 * 1024);
    leveldb::Status status = leveldb::DB::Open(mOptions, path.constData(), &mDB);
    if (!status.ok()) {
        mOpenError = status.ToString();
        error("Couldn't open database %s: %s", path.constData(), mOpenError.constData());
    } else {
        mPath = path;
    }
}

Database::~Database()
{
    delete mDB;
    delete mOptions.block_cache;
    delete mLocationComparator;
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
    mPath.clear();
}

ByteArray Database::openError() const
{
    return mOpenError;
}

void Database::clear()
{
    const Path path = mPath;
    close();
    RTags::removeDirectory(path);
    leveldb::Status status = leveldb::DB::Open(mOptions, path.constData(), &mDB);
    if (!status.ok()) {
        mOpenError = status.ToString();
    } else {
        mPath = path;
    }
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

bool Database::remove(const Slice &key)
{
    return mDB->Delete(mWriteOptions, key.mSlice).ok();
}

Iterator *Database::createIterator() const
{
    return new Iterator(mDB->NewIterator(leveldb::ReadOptions()));
}

