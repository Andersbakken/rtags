#ifndef FileMap_h
#define FileMap_h

/* This file is part of RTags (http://rtags.net).

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
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <rct/Serializer.h>
#include <rct/Rct.h>
#include "Location.h"
#include <functional>

template <typename T> inline static int compare(const T &l, const T &r)
{
    if (l < r)
        return -1;
    if (l > r)
        return 1;
    return 0;
}

template <> inline int compare(const String &l, const String &r)
{
    return l.compare(r);
}

template <> inline int compare(const Location &l, const Location &r)
{
    return l.compare(r);
}

template <typename Key, typename Value>
class FileMap
{
public:
    FileMap()
        : mPointer(0), mSize(0), mCount(0), mKeySize(0), mFD(-1)
    {}

    void init(const char *pointer, size_t size)
    {
        mPointer = pointer;
        mSize = size;
        memcpy(&mCount, mPointer, sizeof(size_t));
        memcpy(&mKeySize, mPointer + sizeof(size_t), sizeof(size_t));
    }

    bool load(const Path &path, String *error = 0)
    {
        eintrwrap(mFD, open(path.constData(), O_RDONLY));
        if (mFD == -1) {
            if (error) {
                *error = Rct::strerror();
                *error << " " << __LINE__;
            }
            return false;
        }
        if (!lock(mFD, Read)) {
            if (error) {
                *error = Rct::strerror();
                *error << " " << __LINE__;
            }

            close(mFD);
            mFD = -1;
            return false;
        }

        struct stat st;
        if (fstat(mFD, &st)) {
            if (error) {
                *error = Rct::strerror();
                *error << " " << __LINE__;
            }
            lock(mFD, Unlock);
            int ret;
            eintrwrap(ret, close(mFD));
            mFD = -1;
            return false;
        }

        const char *pointer = static_cast<const char*>(mmap(0, st.st_size, PROT_READ, MAP_PRIVATE, mFD, 0));
        // error() << errno;//  << mPointer;
        if (pointer == MAP_FAILED) {
            if (error) {
                *error = Rct::strerror();
                *error << " " << __LINE__;
            }
            lock(mFD, Unlock);
            int ret;
            eintrwrap(ret, close(mFD));
            mFD = -1;
            return false;
        }

        init(pointer, st.st_size);
        return true;
    }

    ~FileMap()
    {
        if (mFD != -1) {
            assert(mPointer);
            munmap(const_cast<char*>(mPointer), mSize);
            lock(mFD, Unlock);
            int ret;
            eintrwrap(ret, close(mFD));
        }
    }

    Value value(const Key &key, bool *matched = 0) const
    {
        bool match;
        const int idx = lowerBound(key, &match);
        // error() << "value" << idx << key << match;
        if (matched)
            *matched = match;
        if (match)
            return valueAt(idx);
        return Value();
    }

    int count() const { return mCount; }

    Key keyAt(size_t index) const
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
    size_t lowerBound(const Key &k, bool *match = 0) const
    {
        if (!mCount) {
            if (match)
                *match = false;
            return -1;
        }
        int lower = 0;
        int upper = mCount - 1;

        do {
            const int mid = lower + ((upper - lower) / 2);
            const int cmp = compare<Key>(k, keyAt(mid));
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

        if (lower == static_cast<int>(mCount))
            lower = -1;
        if (match)
            *match = false;
        return lower;
    }

    static String encode(const Map<Key, Value> &map)
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
                out.append(keyData, keySize);
                if (const size_t size = FixedSize<Value>::value) {
                    out.append(reinterpret_cast<const char*>(&value), size);
                } else {
                    out.append(reinterpret_cast<const char *>(&valuesOffset), sizeof(valuesOffset));
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
            memcpy(out.data() + sizeof(size_t), &keySize, sizeof(keySize));
            char buf[keySize];
            size_t entrySize = keySize;
            if (const size_t size = FixedSize<Value>::value) {
                entrySize += size;
            } else {
                entrySize += sizeof(size_t);
            }
            valuesOffset = sizeof(size_t) + sizeof(size_t) + (entrySize * map.size());
            out.reserve(valuesOffset);
            idx = 0;
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

    static bool write(const Path &path, const Map<Key, Value> &map)
    {
        FILE *f = fopen(path.constData(), "w+");
        if (!f && Path::mkdir(path.parentDir(), Path::Recursive)) {
            f = fopen(path.constData(), "w+");
        }
        if (!f)
            return false;
        int err;
        const int fd = fileno(f);
        if (!lock(fd, Write)) {
            fclose(f);
            return false;
        }

        const String data = encode(map);
        bool ret = fwrite(data.constData(), data.size(), 1, f);
        if (ret)
            eintrwrap(err, ftruncate(fd, data.size()));
        ret = lock(fd, Unlock) && ret;
        fclose(f);
        if (!ret)
            unlink(data.constData());
        return ret;
    }
private:
    enum Mode {
        Read = F_RDLCK,
        Write = F_WRLCK,
        Unlock = F_UNLCK
    };
    static bool lock(int fd, Mode mode)
    {
        struct flock fl = { 0, 0, getpid(), static_cast<short>(mode), SEEK_SET };
        int ret;
        eintrwrap(ret, fcntl(fd, F_SETLKW, &fl));
        return ret != -1;
    }
    const char *dataSegment() const { return mPointer + sizeof(size_t) + sizeof(size_t); }

    template <typename T>
    inline static T read(const char *data)
    {
        if (FixedSize<T>::value) {
            T t = T();
            memcpy(&t, data, FixedSize<T>::value);
            return t;
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
    size_t mSize;
    size_t mCount;
    size_t mKeySize;
    int mFD;
};

#endif
