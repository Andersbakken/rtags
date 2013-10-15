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

static inline int intCompare(unsigned int l, unsigned int r)
{
    if (l < r)
        return -1;
    if (l > r)
        return 1;
    return 0;
}
static inline int comparePosition(unsigned int lline, unsigned int lcol, unsigned int rline, unsigned int rcol)
{
    int ret = intCompare(lline, rline);
    if (!ret)
        ret = intCompare(lcol, rcol);
    return ret;
}
class Location
{
public:
    union {
        struct {
            unsigned int mFileId : 19;
            unsigned int mLine : 21;
            unsigned int mColumn : 14;
        } __attribute__ ((__packed__));
        uint64_t mData;
    };

    Location()
        : mData(0)
    {}

    Location(unsigned int fileId, unsigned int line, unsigned int col)
        : mFileId(fileId), mLine(line), mColumn(col)
    {}

    static inline unsigned int fileId(const Path &path)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sPathsToIds.value(path);
    }
    static inline Path path(unsigned int id)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sIdsToPaths.value(id);
    }

#ifndef SINGLE_THREAD
    static inline unsigned int insertFile(const Path &path)
    {
        unsigned int ret;
        {
            std::lock_guard<std::mutex> lock(sMutex);
            unsigned int &id = sPathsToIds[path];
            if (!id) {
                id = ++sLastId;
                sIdsToPaths[id] = path;
            }
            ret = id;
        }

        return ret;
    }
#endif

    inline unsigned int fileId() const { return mFileId; }
    inline unsigned int line() const { return mLine; }
    inline unsigned int column() const { return mColumn; }

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
    inline bool isNull() const { return !mFileId; }
    inline bool isValid() const { return mFileId; }
    inline void clear() { mFileId = mLine = mColumn = 0; mCachedPath.clear(); }
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
        int ret = intCompare(fileId(), other.fileId());
        if (!ret) {
            ret = intCompare(line(), other.line());
            if (!ret)
                ret = intCompare(column(), other.column());
        }
        return ret;
    }
    inline bool operator<(const Location &other) const
    {
        return compare(other) < 0;
    }

    inline bool operator>(const Location &other) const
    {
        return compare(other) > 0;
    }

    String context() const;

    enum KeyFlag {
        NoFlag = 0x0,
        ShowContext = 0x1
    };

    String key(unsigned flags = NoFlag) const;
    bool toKey(char buf[8]) const
    {
        assert(sizeof(Location) == 8);
        if (isNull()) {
            memset(buf, 0, sizeof(Location));
            return false;
        } else {
            memcpy(buf, &mData, sizeof(Location));
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
        unsigned int col;
        unsigned int line;
        memcpy(&col, data.constData() + data.size() - sizeof(col), sizeof(col));
        memcpy(&line, data.constData() + data.size() - sizeof(line) - sizeof(col), sizeof(line));
        const Path path(data.constData(), data.size() - sizeof(col) - sizeof(line));
        const unsigned int fileId = Location::fileId(path);
        if (fileId)
            return Location(fileId, line, col);
        error("Failed to make location from [%s:%d:%d]", path.constData(), line, col);
        return Location();
    }
    static String encodeClientLocation(const String &key)
    {
        char path[PATH_MAX];
        unsigned int line, col;
        if (sscanf(key.constData(), "%[^':']:%d:%d", path, &line, &col) != 3)
            return String();

        Path resolved = Path::resolved(path);
        {
            char buf[8];
            memcpy(buf, &line, sizeof(line));
            memcpy(buf + 4, &col, sizeof(col));
            resolved.append(buf, 8);
        }

        return resolved;
    }

#ifndef SINGLE_THREAD
    static Location fromPathAndOffset(const String &str)
    {
        char path[PATH_MAX];
        unsigned int line, col;
        if (sscanf(str.constData(), "%[^':']:%d:%d", path, &line, &col) != 3)
            return Location();

        const Path resolved = Path::resolved(path);
        return Location(Location::insertFile(resolved), line, col);
    }
#endif
    static Hash<unsigned int, Path> idsToPaths()
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sIdsToPaths;
    }
    static Hash<Path, unsigned int> pathsToIds()
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        return sPathsToIds;
    }
    static void init(const Hash<Path, unsigned int> &pathsToIds)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        sPathsToIds = pathsToIds;
        sLastId = sPathsToIds.size();
        for (Hash<Path, unsigned int>::const_iterator it = sPathsToIds.begin(); it != sPathsToIds.end(); ++it) {
            assert(it->second <= it->second);
            sIdsToPaths[it->second] = it->first;
        }
    }

    static bool set(const Path &path, unsigned int fileId)
    {
#ifndef SINGLE_THREAD
        std::lock_guard<std::mutex> lock(sMutex);
#endif
        // if (sPathsToIds.contains(path)) {
        //     error() << "We've already set" << path << fileId << sPathsToIds.value(path);
        //     return false;
        // } else if (sIdsToPaths.contains(fileId)) {
        //     error() << "We've already set" << path << fileId << sIdsToPaths.value(fileId);
        //     return false;
        // }

        assert(!sPathsToIds.contains(path));
        sPathsToIds[path] = fileId;
        Path &p = sIdsToPaths[fileId];
        if (p.isEmpty())
            p = path;

        return true;
    }
private:
    static Hash<Path, unsigned int> sPathsToIds;
    static Hash<unsigned int, Path> sIdsToPaths;
    static unsigned int sLastId;
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
