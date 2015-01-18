#ifndef RPage_h
#define RPage_h

/* This file is part of RTags.

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include <assert.h>
#include <rct/Serializer.h>
#include <functional>

class RPage
{
public:
    RPage(unsigned char *ptr, int s)
        : pointer(ptr), size(s)
    {}

    unsigned char const *pointer;
    const int size;
};


template <typename T> inline static int compare(T *l, T *r)
{
    return *l - *r;
}

template <typename T> inline static int compare(const char *l, const char *r)
{
    return strcmp(l, r);
}

template <typename T> inline static int compare(Location *l, Location *r)
{
    return l->mData - r->mData;
}

template <typename Key, typename Value>
class Table
{
public:
    Table(const RPage &page)
        : mPage(page), mCount(reinterpret_cast<size_t*>(mPage.pointer)),
          mKeySize(reinterpret_cast<size_t*>(mPage.pointer + sizeof(size_t)))
    {
    }

    Value value(const Key &key) const
    {
        bool match;
        const int idx = lowerBound(key, &match);
        if (match)
            return value(idx);
        return Value();
    }

    int count() const { return mCount; }

    Key key(int index) const
    {
        const unsigned char *ptr = (dataSegment() + (entrySize() * index));
        return read<Key>(ptr);
    }

    Value value(int idx) const
    {
        assert(idx >= 0 && idx < mCount);
        const unsigned char *data = dataSegment() + (entrySize() * idx) + keySize();
        if (FixedSize<Value>::value)
            return read<Value>(data);

        const size_t offset = read<size_t>(data);
        return read<Value>(mPage.pointer + offset);
    }
    int lowerBound(const Key &k, bool *match) const
    {
        int lower = 0;
        int upper = mCount - 1;

        do {
            const int mid = lower + ((upper - lower) / 2);
            const int cmp = compare<Key>(k, key(mid));
            if (cmp < 0) {
                upper = mid - 1;
            } else if (cmp > 0) {
                lower = mid + 1;
            } else {
                if (match)
                    *match = true;
                return mid;
            }
        } while (lower <= upper);

        if (lower == mCount || compare(key, key(lower)) < 0)
            --lower;
        if (match)
            *match = false;
        return lower;
    }
    static String create(const Map<Key, Value> &map)
    {
        String out;
        String values;
        Serializer serializer(out);
        size_t keySize = 0;
        out << map.size();
        out << 0; // keysize

        // auto encodeKey = [&out, &serializer, &keySize](const Key &key) { out.append(reinterpret_cast<const char*>(&key), FixedSize<Key>::value); };
        // if (!FixedSize<Key>::value) {
        //     encodeKey = [&out, &serializer, &keySize](const Key &key) {
        //         const int pos = out.size();
        //         serializer << key;
        //         keySize = std::max<int>(out.size() - pos, keySize);
        //     };
        // }
        // // auto encodeKey = FixedSize<Key>::value ? encodeKeyFixed : encodeKeyVariable;

        if (!FixedSize<Value>::value)
            out += values;
        return out;
    }
private:
    template <typename std::enable_if<FixedSize<Key>::value != 0>::type * = nullptr>
    inline void encodeKey(const Key &key, String &out, Serializer &, size_t &)
    {
        out.append(reinterpret_cast<const char*>(&key), FixedSize<Key>::value);
    }

    template <typename std::enable_if<FixedSize<Key>::value == 0>::type * = nullptr>
    inline void encodeKey(const Key &key, String &out, Serializer &serializer, size_t &keySize)
    {
        const int pos = out.size();
        serializer << key;
        keySize = std::max<int>(out.size() - pos, keySize);
    }

    unsigned char *dataSegment() const
    {
        return mPage.pointer + sizeof(size_t) + sizeof(size_t);
    }
    template <typename T>
    inline static T read(const unsigned char *data)
    {
        if (FixedSize<Key>::value) {
            return *reinterpret_cast<const T*>(data);
        }
        Deserializer deserializer(data, INT_MAX);
        T t;
        deserializer << t;
        return t;
    }
    size_t keySize() const
    {
        return mKeySize ? mKeySize : FixedSize<Key>::value;
    }
    size_t entrySize() const
    {
        if (FixedSize<Value>::value) {
            return keySize() + sizeof(Value);
        } else {
            return keySize() + sizeof(size_t);
        }
    }

    RPage mPage;
    const size_t mCount;
    const size_t mKeySize;
};

#endif
