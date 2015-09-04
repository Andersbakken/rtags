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
#include <rct/StackBuffer.h>
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
        : mPointer(0), mSize(0), mCount(0), mValuesOffset(0), mFD(-1)
    {}

    void init(const char *pointer, uint32_t size)
    {
        mPointer = pointer;
        mSize = size;
        memcpy(&mCount, mPointer, sizeof(uint32_t));
        memcpy(&mValuesOffset, mPointer + sizeof(uint32_t), sizeof(uint32_t));
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

    Key keyAt(uint32_t index) const
    {
        assert(index >= 0 && index < mCount);
        return read<Key>(keysSegment(), index);
    }

    Value valueAt(uint32_t index) const
    {
        assert(index >= 0 && index < mCount);
        return read<Value>(valuesSegment(), index);
    }

    uint32_t lowerBound(const Key &k, bool *match = 0) const
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
        serializer << static_cast<uint32_t>(map.size());
        uint32_t valuesOffset;
        if (uint32_t size = FixedSize<Key>::value) {
            valuesOffset = ((static_cast<uint32_t>(map.size()) * size) + (sizeof(uint32_t) * 2));
            serializer << valuesOffset;
            for (const std::pair<Key, Value> &pair : map) {
                out.append(reinterpret_cast<const char*>(&pair.first), size);
            }
        } else {
            serializer << static_cast<uint32_t>(0); // values offset
            uint32_t offset = sizeof(uint32_t) * 2 + (map.size() * sizeof(uint32_t));
            String keyData;
            Serializer keySerializer(keyData);
            for (const std::pair<Key, Value> &pair : map) {
                const uint32_t pos = offset + keyData.size();
                out.append(reinterpret_cast<const char*>(&pos), sizeof(pos));
                keySerializer << pair.first;
            }
            out.append(keyData);
            valuesOffset = out.size();
            memcpy(out.data() + sizeof(uint32_t), &valuesOffset, sizeof(valuesOffset));
        }
        assert(valuesOffset == out.size());

        if (uint32_t size = FixedSize<Value>::value) {
            for (const std::pair<Key, Value> &pair : map) {
                out.append(reinterpret_cast<const char*>(&pair.second), size);
            }
        } else {
            const uint32_t encodedValuesOffset = valuesOffset + (sizeof(uint32_t) * map.size());
            String valueData;
            Serializer valueSerializer(valueData);
            for (const std::pair<Key, Value> &pair : map) {
                const uint32_t pos = encodedValuesOffset + valueData.size();
                out.append(reinterpret_cast<const char*>(&pos), sizeof(pos));
                valueSerializer << pair.second;
            }
            out.append(valueData);

        }
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
    const char *valuesSegment() const { return mPointer + mValuesOffset; }
    const char *keysSegment() const { return mPointer + (sizeof(uint32_t) * 2); }

    template <typename T>
    inline T read(const char *base, uint32_t index) const
    {
        if (const uint32_t size = FixedSize<T>::value) {
            T t = T();
            memcpy(&t, base + (index * size), FixedSize<T>::value);
            return t;
        }
        uint32_t offset;
        memcpy(&offset, base + (sizeof(uint32_t) * index), sizeof(offset));
        Deserializer deserializer(mPointer + offset, INT_MAX);
        T t;
        deserializer >> t;
        return t;
    }

const char *mPointer;
    uint32_t mSize;
    uint32_t mCount;
    uint32_t mValuesOffset;
    int mFD;
};

#endif
