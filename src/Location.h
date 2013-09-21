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

#ifndef Location_h
#define Location_h

#include <rct/String.h>
#include <rct/Log.h>
#include <rct/Path.h>
#include <rct/Serializer.h>
#include <mutex>
#include <assert.h>
#include <clang-c/Index.h>
#include <stdio.h>

class Location
{
public:
    uint64_t mData;
    Location()
        : mData(0)
    {}
    Location(uint64_t data)
        : mData(data)
    {}
    Location(uint32_t fileId, uint32_t offset)
        : mData(uint64_t(offset) << 32 | fileId)
    {}

    inline bool operator!() const
    {
        return !mData;
    }

    static inline uint32_t fileId(const Path &path)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sPathsToIds.value(path);
    }
    static inline Path path(uint32_t id)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sIdsToPaths.value(id);
    }

#ifndef SINGLE_THREAD
    static inline uint32_t insertFile(const Path &path)
    {
        uint32_t ret;
        {
            std::lock_guard<std::mutex> lock(sMutex);
            uint32_t &id = sPathsToIds[path];
            if (!id) {
                id = ++sLastId;
                sIdsToPaths[id] = path;
            }
            ret = id;
        }

        return ret;
    }
#endif

    inline uint32_t fileId() const { return uint32_t(mData); }
    inline uint32_t offset() const { return uint32_t(mData >> 32); }

    inline Path path() const
    {
        if (mCachedPath.isEmpty()) {
#ifndef SINGLE_THREAD
            std::lock_guard<std::mutex> lock(sMutex);
#endif
            mCachedPath = sIdsToPaths.value(fileId());
        }
        return mCachedPath;
    }
    inline bool isNull() const { return !mData; }
    inline bool isValid() const { return mData; }
    inline void clear() { mData = 0; mCachedPath.clear(); }
#ifndef SINGLE_THREAD
    inline bool operator==(const String &str) const
    {
        const Location fromPath = Location::fromPathAndOffset(str);
        return operator==(fromPath);
    }
#endif
    inline bool operator==(const Location &other) const { return mData == other.mData; }
    inline bool operator!=(const Location &other) const { return mData != other.mData; }
    inline int compare(const Location &other) const
    {
        int diff = other.fileId() - fileId();
        if (diff < 0) {
            return -1;
        } else if (diff > 0) {
            return 1;
        }
        diff = other.offset() - offset();
        if (diff < 0) {
            return -1;
        } else if (diff > 0) {
            return 1;
        }
        return 0;
    }
    inline bool operator<(const Location &other) const
    {
        const int off = other.fileId() - fileId();
        if (off < 0) {
            return true;
        } else if (off > 0) {
            return false;
        }
        return offset() < other.offset();
    }

    inline bool operator>(const Location &other) const
    {
        const int off = other.fileId() - fileId();
        if (off < 0) {
            return false;
        } else if (off > 0) {
            return true;
        }
        return offset() > other.offset();
    }

    String context(int *column = 0) const;
    bool convertOffset(int &line, int &col) const;

    enum KeyFlag {
        NoFlag = 0x0,
        Padded = 0x1,
        ShowContext = 0x2,
        ShowLineNumbers = 0x4
    };

    String key(unsigned flags = NoFlag) const;
    bool toKey(char buf[8]) const
    {
        if (isNull()) {
            memset(buf, 0, 8);
            return false;
        } else {
            memcpy(buf, &mData, sizeof(mData));
            return true;
        }
    }

    static Location fromKey(const char *data)
    {
        Location ret;
        memcpy(&ret.mData, data, sizeof(ret.mData));
        return ret;
    }

    static Location decodeClientLocation(const String &data)
    {
        uint32_t offset;
        memcpy(&offset, data.constData() + data.size() - sizeof(offset), sizeof(offset));
        const Path path(data.constData(), data.size() - sizeof(offset));
        const uint32_t fileId = Location::fileId(path);
        if (fileId)
            return Location(fileId, offset);
        error("Failed to make location from [%s,%d]", path.constData(), offset);
        return Location();
    }
    static String encodeClientLocation(const String &key)
    {
        const int lastComma = key.lastIndexOf(',');
        if (lastComma <= 0 || lastComma + 1 >= key.size())
            return String();

        char *endPtr;
        uint32_t offset = strtoull(key.constData() + lastComma + 1, &endPtr, 10);
        if (*endPtr != '\0')
            return String();
        Path path = Path::resolved(key.left(lastComma));
        String out;
        {
            out = path;
            char buf[4];
            memcpy(buf, &offset, sizeof(buf));
            out += String(buf, 4);
        }

        return out;
    }

#ifndef SINGLE_THREAD
    static Location fromPathAndOffset(const String &pathAndOffset)
    {
        const int comma = pathAndOffset.lastIndexOf(',');
        if (comma <= 0 || comma + 1 == pathAndOffset.size()) {
            error("Can't create location from this: %s", pathAndOffset.constData());
            return Location();
        }
        bool ok;
        const uint32_t fileId = String(pathAndOffset.constData() + comma + 1, pathAndOffset.size() - comma - 1).toULongLong(&ok);
        if (!ok) {
            error("Can't create location from this: %s", pathAndOffset.constData());
            return Location();
        }
        return Location(Location::insertFile(Path(pathAndOffset.left(comma))), fileId);
    }
#endif
    static Map<uint32_t, Path> idsToPaths()
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sIdsToPaths;
    }
    static Map<Path, uint32_t> pathsToIds()
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sPathsToIds;
    }
    static void init(const Map<Path, uint32_t> &pathsToIds)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        sPathsToIds = pathsToIds;
        sLastId = sPathsToIds.size();
        for (Map<Path, uint32_t>::const_iterator it = sPathsToIds.begin(); it != sPathsToIds.end(); ++it) {
            assert(it->second <= it->second);
            sIdsToPaths[it->second] = it->first;
        }
    }

    static bool set(const Path &path, uint32_t fileId)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        // if (sPathsToIds.contains(path)) {
        //     error() << "We've already set" << path << fileId << sPathsToIds.value(path)
        //             << sPathsToIds;
        //     return false;
        // }

        assert(!sPathsToIds.contains(path));
        assert(!sIdsToPaths.contains(fileId));
        sPathsToIds[path] = fileId;
        sIdsToPaths[fileId] = path;

        return true;
    }
private:
    static Map<Path, uint32_t> sPathsToIds;
    static Map<uint32_t, Path> sIdsToPaths;
    static uint32_t sLastId;
    static std::mutex sMutex;
    mutable Path mCachedPath;
};

template <> inline int fixedSize(const Location &)
{
    return sizeof(uint64_t);
}

template <> inline Serializer &operator<<(Serializer &s, const Location &t)
{
    s.write(reinterpret_cast<const char*>(&t.mData), sizeof(uint64_t));
    return s;
}

template <> inline Deserializer &operator>>(Deserializer &s, Location &t)
{
    s.read(reinterpret_cast<char*>(&t), sizeof(uint64_t));
    return s;
}

static inline Log operator<<(Log dbg, const Location &loc)
{
    const String out = "Location(" + loc.key() + ")";
    return (dbg << out);
}

#endif
