#ifndef Serializer_h
#define Serializer_h

#include "ByteArray.h"
#include "List.h"
#include "Log.h"
#include "Map.h"
#include "Path.h"
#include "Set.h"
#include <assert.h>
#include <stdint.h>

class Serializer
{
public:
    Serializer(ByteArray &out)
        : mOut(&out), mOutFile(0)
    {}

    Serializer(FILE *f)
        : mOut(0), mOutFile(f)
    {
        assert(f);
    }
    bool write(const char *data, int len)
    {
        assert(len > 0);
        if (mOut) {
            mOut->append(data, len);
            return true;
        } else {
            assert(mOutFile);
            const size_t ret = fwrite(data, sizeof(char), len, mOutFile);
            return (ret == static_cast<size_t>(len));
        }
    }
private:
    ByteArray *mOut;
    FILE *mOutFile;
};

class Deserializer
{
public:
    Deserializer(const char *data, int length)
        : mData(data), mLength(length), mPos(0), mFile(0)
    {}

    Deserializer(FILE *file)
        : mData(0), mLength(0), mFile(file)
    {
        assert(file);
    }
    int read(char *target, int len)
    {
        assert(len > 0);
        if (mData) {
            assert(mPos + len <= mLength);
            memcpy(target, mData + mPos, len);
            mPos += len;
            return len;
        } else {
            assert(mFile);
            return fread(target, sizeof(char), len, mFile);
        }
    }

    int pos() const
    {
        return mPos;
    }
private:
    const char *mData;
    const int mLength;
    int mPos;
    FILE *mFile;
};

template <typename T>
Serializer &operator<<(Serializer &s, const T &t)
{
    YouNeedToDeclareLeftShiftOperators(t);
    return s;
}

template <typename T>
Deserializer &operator>>(Deserializer &s, T &t)
{
    YouNeedToDeclareRightShiftOperators(t);
    return s;
}

template <typename T> inline int fixedSize(const T &)
{
    return 0;
}
#define DECLARE_NATIVE_TYPE(type)                                       \
    template <> inline int fixedSize(const type &)                      \
    {                                                                   \
        return sizeof(type);                                            \
    }                                                                   \
    template <> inline Serializer &operator<<(Serializer &s,            \
                                              const type &t)            \
    {                                                                   \
        s.write(reinterpret_cast<const char*>(&t), sizeof(type));       \
        return s;                                                       \
    }                                                                   \
    template <> inline Deserializer &operator>>(Deserializer &s,        \
                                                type &t)                \
    {                                                                   \
        s.read(reinterpret_cast<char*>(&t), sizeof(type));              \
        return s;                                                       \
    }


DECLARE_NATIVE_TYPE(bool);
DECLARE_NATIVE_TYPE(char);
DECLARE_NATIVE_TYPE(unsigned char);
DECLARE_NATIVE_TYPE(uint16_t);
DECLARE_NATIVE_TYPE(int16_t);
DECLARE_NATIVE_TYPE(uint32_t);
DECLARE_NATIVE_TYPE(int32_t);
DECLARE_NATIVE_TYPE(uint64_t);
DECLARE_NATIVE_TYPE(int64_t);
#ifdef OS_Darwin
DECLARE_NATIVE_TYPE(long);
#endif
#ifndef __x86_64__
DECLARE_NATIVE_TYPE(time_t);
#endif

template <>
inline Serializer &operator<<(Serializer &s, const ByteArray &byteArray)
{
    const int size = byteArray.size();
    s << size;
    if (size)
        s.write(byteArray.constData(), byteArray.size()); // do I need to write out null terminator?
    return s;
}


template <>
inline Serializer &operator<<(Serializer &s, const Path &path)
{
    const int size = path.size();
    s << size;
    if (size)
        s.write(path.constData(), size);
    return s;
}


template <typename T>
Serializer &operator<<(Serializer &s, const List<T> &list)
{
    const int size = list.size();
    s << size;
    if (size) {
        const int fixed = fixedSize<T>(T());
        if (fixed) {
            s.write(reinterpret_cast<const char*>(list.data()), fixed * size);
        } else {
            for (int i=0; i<size; ++i) {
                s << list.at(i);
            }
        }
    }
    return s;
}

template <typename Key, typename Value>
Serializer &operator<<(Serializer &s, const Map<Key, Value> &map)
{
    const int size = map.size();
    s << size;
    for (typename Map<Key, Value>::const_iterator it = map.begin(); it != map.end(); ++it) {
        s << it->first << it->second;
    }
    return s;
}

template <typename First, typename Second>
Serializer &operator<<(Serializer &s, const std::pair<First, Second> &pair)
{
    s << pair.first << pair.second;
    return s;
}


template <typename T>
Serializer &operator<<(Serializer &s, const Set<T> &set)
{
    const int size = set.size();
    s << size;
    for (typename Set<T>::const_iterator it = set.begin(); it != set.end(); ++it) {
        s << *it;
    }
    return s;
}

template <typename Key, typename Value>
Deserializer &operator>>(Deserializer &s, Map<Key, Value> &map)
{
    int size;
    s >> size;
    map.clear();
    if (size) {
        Key key;
        Value value;
        for (int i=0; i<size; ++i) {
            s >> key >> value;
            map[key] = value;
        }
    }
    return s;
}

template <typename T>
Deserializer &operator>>(Deserializer &s, List<T> &list)
{
    int size;
    s >> size;
    list.resize(size);
    if (size) {
        const int fixed = fixedSize<T>(T());
        if (fixed) {
            s.read(reinterpret_cast<char*>(list.data()), fixed * size);
        } else {
            for (int i=0; i<size; ++i) {
                s >> list[i];
            }
        }
    }
    return s;
}

template <typename T>
Deserializer &operator>>(Deserializer &s, Set<T> &set)
{
    set.clear();
    int size;
    s >> size;
    if (size) {
        T t;
        for (int i=0; i<size; ++i) {
            s >> t;
            set.insert(t);
        }
    }
    return s;
}

template <>
inline Deserializer &operator>>(Deserializer &s, ByteArray &byteArray)
{
    int size;
    s >> size;
    byteArray.resize(size);
    if (size)
        s.read(byteArray.data(), size);
    return s;
}

template <typename First, typename Second>
Deserializer &operator>>(Deserializer &s, std::pair<First, Second> &pair)
{
    s >> pair.first >> pair.second;
    return s;
}

template <>
inline Deserializer &operator>>(Deserializer &s, Path &path)
{
    int size;
    s >> size;
    path.resize(size);
    if (size)
        s.read(path.data(), size);
    return s;
}

#endif
