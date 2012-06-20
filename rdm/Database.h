#ifndef Database_h
#define Database_h

#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#include <QtCore>
#include <Serializer.h>
#include "Location.h"
#include "CursorInfo.h"

struct Slice {
    Slice(const std::string &str);
    Slice(const ByteArray &d);
    Slice(const char *d = 0, int s = -1);
    const char *data() const;
    int size() const;
    void clear();
    bool operator==(const Slice &other) const;
    bool operator!=(const Slice &other) const;
    ByteArray byteArray() const { return ByteArray(data(), size()); }
private:
    Slice(const leveldb::Slice &slice);
    leveldb::Slice mSlice;
    friend class Database;
    friend class Iterator;
    friend struct Batch;
};

static inline Log &operator<<(Log &dbg, const Slice &slice)
{
    dbg << std::string(slice.data(), slice.size()).c_str();
    return dbg;
}

template <typename T> ByteArray encode(const T &t)
{
    ByteArray out;
    Serializer s(out);
    s << t;
    return out;
}

template <typename T> T decode(const Slice &slice)
{
    Deserializer ds(slice.data(), slice.size());
    T t;
    ds >> t;
    return t;
}

template <> inline ByteArray encode(const Set<Location> &locations)
{
    ByteArray out(locations.size() * sizeof(uint64_t), '\0');
    uint64_t *ptr = reinterpret_cast<uint64_t*>(out.data());
    for (Set<Location>::const_iterator it = locations.begin(); it != locations.end(); ++it) {
        *ptr++ = (*it).mData;
    }
    return out;
}

template <> inline Set<Location> decode(const Slice &slice)
{
    Set<Location> ret;
    const uint64_t *ptr = reinterpret_cast<const uint64_t*>(slice.data());
    const int count = slice.size() / sizeof(uint64_t);
    for (int i=0; i<count; ++i) {
        ret.insert(Location(*ptr++));
    }
    return ret;
}

template <> inline ByteArray encode(const CursorInfo &info)
{
    // null-terminated symbolName, uint32_t(symbolLength), uint32_t(kind), unsigned char(isDefinition), uint64_t(target.location), uint64_t(refs)...
    ByteArray out(info.symbolName.size() + 1 + (sizeof(uint32_t) * 2) + sizeof(unsigned char)
                   + (sizeof(uint64_t) * (1 + info.references.size())), '\0');
    memcpy(out.data(), info.symbolName.constData(), info.symbolName.size() + 1);
    uint32_t *ptr = reinterpret_cast<uint32_t*>(out.data() + (info.symbolName.size() + 1));
    *ptr++ = info.symbolLength;
    *ptr++ = info.kind;
    unsigned char *isDefinitionPtr = reinterpret_cast<unsigned char*>(ptr);
    *isDefinitionPtr++ = info.isDefinition;
    uint64_t *locPtr = reinterpret_cast<uint64_t*>(isDefinitionPtr);
    *locPtr++ = info.target.mData;
    foreach(const Location &loc, info.references) {
        *locPtr++ = loc.mData;
    }
    return out;
}

template <> inline CursorInfo decode(const Slice &slice)
{
    CursorInfo ret;
    ret.symbolName = ByteArray(slice.data()); // 0-terminated
    const uint32_t *ptr = reinterpret_cast<const uint32_t*>(slice.data() + ret.symbolName.size() + 1);
    ret.symbolLength = *ptr++;
    ret.kind = static_cast<CXCursorKind>(*ptr++);
    const unsigned char *isDefinitionPtr = reinterpret_cast<const unsigned char*>(ptr);
    ret.isDefinition = *isDefinitionPtr++;
    const uint64_t *locPtr = reinterpret_cast<const uint64_t*>(isDefinitionPtr);
    const int count = ((slice.size() - ret.symbolName.size() - sizeof(char) - (sizeof(uint32_t) * 2) - sizeof(unsigned char)) / sizeof(uint64_t));
    ret.target.mData = *locPtr++;
    for (int i=0; i<count - 1; ++i) {
        const Location loc(*locPtr++);
        ret.references.insert(loc);
    }
    return ret;
}


class Iterator
{
    Iterator(leveldb::Iterator *iterator);
public:
    ~Iterator();
    void seekToFirst();
    void seekToLast();
    bool isValid() const;
    void next();
    void previous();
    Slice key() const;
    Slice rawValue() const;
    void seek(const Slice &slice);
    template <typename T> T value() const { return decode<T>(rawValue()); }
private:
    leveldb::Iterator *mIterator;
    friend class Database;
};

class LocationComparator;
class Database
{
public:
    Database(const char *path, int cacheSizeMB, bool locationKeys);
    ~Database();
    void lockForRead() { mLock.lockForRead(); }
    void lockForWrite() { mLock.lockForWrite(); }
    void unlock() { mLock.unlock(); }
    bool isOpened() const;
    void close();
    ByteArray openError() const;
    std::string rawValue(const Slice &key, bool *ok = 0) const;
    template <typename T> T value(const Slice &key, bool *ok = 0) {
        const std::string val = rawValue(key, ok);
        if (!val.empty())
            return decode<T>(val);
        return T();
    }
    int setRawValue(const Slice &key, const Slice &value);
    template <typename T> int setValue(const Slice &key, const T &t) { return setRawValue(key, encode(t)); }
    bool contains(const Slice &key) const;
    void remove(const Slice &key);
    Iterator *createIterator() const;
private:
    QReadWriteLock mLock;
    leveldb::DB *mDB;
    const leveldb::WriteOptions mWriteOptions;
    ByteArray mOpenError;
    LocationComparator *mLocationComparator;
    friend struct Batch;
};

struct Batch {
    enum { BatchThreshold = 1024 * 1024 };
    Batch(Database *d);
    ~Batch();
    int flush();
    template <typename T> int add(const Slice &key, const T &t)
    {
        const ByteArray encoded = encode<T>(t);
        return addEncoded(key, Slice(encoded));
    }

    void remove(const Slice &key);
    int size() const { return mSize; }
    int total() const { return mTotal; }
    int addEncoded(const Slice &key, const Slice &data);
private:
    Database *mDB;
    int mSize, mTotal;
    leveldb::WriteBatch mBatch;
};


#endif
