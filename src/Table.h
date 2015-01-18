#ifndef Table_h
#define Table_h

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

template <typename T> inline static int compare(T l, T r)
{
    return l - r;
}

inline static int compare(const String &l, const String &r)
{
    return l.compare(r);
}

inline static int compare(const Location &l, const Location &r)
{
    return l.mData - r.mData;
}

template <typename Key, typename Value>
class Table
{
public:
    Table(const char *pointer, size_t size)
        : mPointer(pointer), mSize(size), mCount(*reinterpret_cast<const size_t*>(pointer)),
          mKeySize(*reinterpret_cast<const size_t*>(pointer + sizeof(size_t)))
    {
    }

    Value value(const Key &key) const
    {
        bool match;
        const int idx = lowerBound(key, &match);
        error() << "value" << idx << key << match;
        if (match)
            return value(idx);
        return Value();
    }

    int count() const { return mCount; }

    Key key(size_t index) const
    {
        const char *ptr = (dataSegment() + (entrySize() * index));
        return read<Key>(ptr);
    }

    Value valueAt(size_t idx) const
    {
        assert(idx >= 0 && idx < mCount);
        const char *data = dataSegment() + (entrySize() * idx) + keySize();
        if (FixedSize<Value>::value)
            return read<Value>(data);

        const size_t offset = read<size_t>(data);
        return read<Value>(mPointer + offset);
    }
    size_t lowerBound(const Key &k, bool *match) const
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

        if (lower == static_cast<int>(mCount) || compare(k, key(lower)) < 0)
            --lower;
        if (match)
            *match = false;
        return lower;
    }
    static String create(const Map<Key, Value> &map)
    {
        String out;
        Serializer serializer(out);

        String values;
        Serializer valuesSerializer(values);
        size_t valuesOffset = 0;

        size_t keySize = 0;
        serializer << static_cast<size_t>(map.size());
        serializer << static_cast<size_t>(0); // keysize

        auto encodePair = [&out, &valuesSerializer, &values, &valuesOffset](const char *keyData, size_t keySize, const Value &value)
        {
            // printf("encoding a key %d [%x, %x]\n", *reinterpret_cast<const int*>(keyData),
            //        keyData[0], keyData[1]);
            out.append(keyData, keySize);
            if (const size_t size = FixedSize<Value>::value) {
                // error() << "Encoding a value" << value;
                out.append(reinterpret_cast<const char*>(&value), size);
            } else {
                values.append(reinterpret_cast<const char*>(&valuesOffset), sizeof(valuesOffset));
                const int old = values.size();
                valuesSerializer << value;
                valuesOffset += (values.size() - old);
            }
        };

        if (!FixedSize<Key>::value) {
            List<String> keys(map.size());
            int idx = 0;
            for (const auto &pair : map) {
                String &str = keys[idx++];
                Serializer s(str);
                s << pair.first;
                keySize = std::max<size_t>(str.size(), keySize);
            }
            *reinterpret_cast<size_t*>(out.data() + sizeof(size_t)) = keySize;
            char buf[keySize];
            size_t entrySize = keySize;
            if (const size_t size = FixedSize<Value>::value) {
                entrySize += size;
            } else {
                entrySize += sizeof(size_t);
            }
            valuesOffset = sizeof(size_t) + sizeof(size_t) + (entrySize * map.size());
            out.reserve(valuesOffset);
            for (const auto &pair : map) {
                memset(buf, 0, sizeof(buf));
                const String &str = keys[idx++];
                memcpy(buf, str.data(), str.size()); // no need to copy the \0 :-)
                encodePair(buf, keySize, pair.second);
            }
        } else {
            size_t entrySize = FixedSize<Key>::value;
            if (const size_t size = FixedSize<Value>::value) {
                entrySize += size;
            } else {
                entrySize += sizeof(size_t);
            }
            valuesOffset = sizeof(size_t) + sizeof(size_t) + (entrySize * map.size());
            out.reserve(valuesOffset);
            for (const auto &pair : map) {
                // error() << "Encoding a key" << pair.first;
                encodePair(reinterpret_cast<const char*>(&pair.first), FixedSize<Key>::value, pair.second);
            }
        }

        if (!FixedSize<Value>::value)
            out += values;
        return out;
    }
private:
    const char *dataSegment() const { return mPointer + sizeof(size_t) + sizeof(size_t); }

    template <typename T>
    inline static T read(const char *data)
    {
        if (FixedSize<T>::value) {
            return *reinterpret_cast<const T*>(data);
        }
        Deserializer deserializer(data, INT_MAX);
        T t;
        deserializer >> t;
        return t;
    }
    size_t keySize() const { return mKeySize ? mKeySize : FixedSize<Key>::value; }
    size_t entrySize() const
    {
        if (FixedSize<Value>::value) {
            return keySize() + sizeof(Value);
        } else {
            return keySize() + sizeof(size_t);
        }
    }

    const char *mPointer;
    const size_t mSize;
    const size_t mCount;
    const size_t mKeySize;
};

#endif
