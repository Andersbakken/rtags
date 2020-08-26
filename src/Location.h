/* This file is part of RTags (https://github.com/Andersbakken/rtags).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef Location_h
#define Location_h

#include <assert.h>
#include <clang-c/Index.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <functional>
#include <type_traits>
#include <utility>
#if defined(OS_Linux)
#include <linux/limits.h>
#elif defined(OS_Darwin)
#include <sys/syslimits.h>
#endif
#ifndef RTAGS_SINGLE_THREAD
#include <mutex>

#define LOCK() const std::lock_guard<std::mutex> lock(sMutex)
#else
#define LOCK() do {} while (0)
#endif

#include "rct/Flags.h"
#include "rct/Log.h"
#include "rct/Path.h"
#include "rct/Serializer.h"
#include "rct/String.h"
#include "rct/StackBuffer.h"
#include "rct/Hash.h"

static inline int intCompare(uint32_t l, uint32_t r)
{
    if (l < r)
        return -1;
    if (l > r)
        return 1;
    return 0;
}
static inline int comparePosition(uint32_t lline, uint32_t lcol, uint32_t rline, uint32_t rcol)
{
    int ret = intCompare(lline, rline);
    if (!ret)
        ret = intCompare(lcol, rcol);
    return ret;
}

class Location
{
public:
    uint64_t value;

    Location()
        : value(0)
    {}

    Location(uint32_t file, uint32_t l, uint32_t col)
        : value((static_cast<uint64_t>(col) << (FileBits + LineBits)) | (static_cast<uint64_t>(l) << (FileBits)) | file)
    {
    }

    static inline uint32_t fileId(const Path &path)
    {
        LOCK();
        return sPathsToIds.value(path);
    }
    static inline Path path(uint32_t id)
    {
        LOCK();
        return sIdsToPaths.value(id);
    }

    static uint32_t lastId()
    {
        LOCK();
        return sLastId;
    }

    static uint32_t count()
    {
        LOCK();
        return sIdsToPaths.size();
    }

    static inline uint32_t insertFile(const Path &path)
    {
        bool save = false;
        (void)save;
        assert(path.isAbsolute());
        assert(!path.contains("/../"));
        // in the case of Source::compilerId path can be a symlink
        uint32_t ret;
        {
            LOCK();
            uint32_t &id = sPathsToIds[path];
            if (!id) {
                id = ++sLastId;
                sIdsToPaths[id] = path;
                save = true;
            }
            ret = id;
        }
#ifndef RTAGS_SINGLE_THREAD
        if (save)
            saveFileIds();
#endif
        return ret;
    }

    inline uint32_t fileId() const { return static_cast<uint32_t>(value & FILEID_MASK); }
    inline uint32_t line() const { return static_cast<uint32_t>((value & LINE_MASK) >> FileBits); }
    inline uint32_t column() const { return static_cast<uint32_t>((value & COLUMN_MASK) >> (FileBits + LineBits)); }

    inline Path path() const
    {
        LOCK();
        return sIdsToPaths.value(fileId());
    }
    inline bool isNull() const { return !value; }
    inline bool isValid() const { return value; }
    inline void clear() { value = 0; }
    inline bool operator==(Location other) const { return value == other.value; }
    inline bool operator!=(Location other) const { return value != other.value; }
    inline int compare(Location other) const
    {
        int ret = intCompare(fileId(), other.fileId());
        if (!ret) {
            ret = intCompare(line(), other.line());
            if (!ret)
                ret = intCompare(column(), other.column());
        }
        return ret;
    }
    inline bool operator<(Location other) const
    {
        return compare(other) < 0;
    }

    inline bool operator<=(Location other) const
    {
        return compare(other) <= 0;
    }

    inline bool operator>(Location other) const
    {
        return compare(other) > 0;
    }

    inline bool operator>=(Location other) const
    {
        return compare(other) >= 0;
    }

    enum ToStringFlag {
        NoFlag = 0x0,
        ShowContext = 0x1,
        NoColor = 0x2,
        AbsolutePath = 0x4,
        ConvertToRelative = 0x8
    };

    String toString(Flags<ToStringFlag> flags = NoFlag, Hash<Path, String> *contextCache = nullptr) const;
    String context(Flags<ToStringFlag> flags, Hash<Path, String> *cache = nullptr) const;

    inline String debug() const;

    enum DecodeFlag {
        NoDecodeFlag = 0x0,
        CreateLocation = 0x1
    };

    static Location decode(const String &data, DecodeFlag flag = NoDecodeFlag)
    {
        uint32_t col;
        uint32_t line;
        memcpy(&col, data.constData() + data.size() - sizeof(col), sizeof(col));
        memcpy(&line, data.constData() + data.size() - sizeof(line) - sizeof(col), sizeof(line));
        Path path(data.constData(), data.size() - sizeof(col) - sizeof(line));
        uint32_t fileId = Location::fileId(path);
        if (!fileId) {
            path.resolve();
            fileId = Location::fileId(path);
        }
        if (!fileId && flag == CreateLocation)
            fileId = Location::insertFile(path);
        if (fileId)
            return Location(fileId, line, col);
        error("%s:%d:%d is not indexed", path.constData(), line, col);
        return Location();
    }
    static bool parse(const String &str, const Path &pwd, Path::ResolveMode mode,
                      Path *path, uint32_t *line, uint32_t *col)
    {
        const size_t lastColon = str.lastIndexOf(':', str.size() - 2);
        if (lastColon == String::npos)
            return false;
        const size_t secondLastColon = str.lastIndexOf(':', lastColon - 1);
        if (secondLastColon == String::npos)
            return false;
        const char *cstr = str.constData();
        char *end;
        assert(line);
        *line = static_cast<uint32_t>(strtoul(cstr + secondLastColon + 1, &end, 10));
        if (*end != ':' || end == str + secondLastColon + 1)
            return false;
        assert(col);
        *col = static_cast<uint32_t>(strtoul(cstr + lastColon + 1, &end, 10));
        if ((*end && *end != ':') || end == str + lastColon + 1)
            return false;

        *path = Path::resolved(str.left(secondLastColon), mode, pwd);
        return path->isFile();
    }

    static String encode(const String &str, const Path &pwd = Path())
    {
        uint32_t line, col;
        Path path;
        if (!parse(str, pwd, Path::MakeAbsolute, &path, &line, &col))
            return String();
        char buf[8];
        memcpy(buf, &line, sizeof(line));
        memcpy(buf + 4, &col, sizeof(col));
        path.append(buf, 8);
        return std::move(path);
    }

    static Location fromPathLineAndColumn(const String &str, const Path &pwd = Path())
    {
        uint32_t line, col;
        Path path;
        if (!parse(str, pwd, Path::RealPath, &path, &line, &col))
            return Location();

        const uint32_t fileId = Location::fileId(path);
        if (!fileId)
            return Location();
        return Location(fileId, line, col);
    }
    static Hash<uint32_t, Path> idsToPaths()
    {
        LOCK();
        return sIdsToPaths;
    }
    static Hash<Path, uint32_t> pathsToIds()
    {
        LOCK();
        return sPathsToIds;
    }

    static void iterate(std::function<void(const Path &, uint32_t)> func)
    {
        LOCK();
        for (const auto &it : sPathsToIds) {
            func(it.first, it.second);
        }
    }
    static bool init(const Hash<Path, uint32_t> &pathsToIds);
    static void init(const Hash<uint32_t, Path> &idsToPaths);

    static void set(const Path &path, uint32_t fileId)
    {
        LOCK();
        uint32_t &refId = sPathsToIds[path];
        assert(!refId || refId == fileId);
        refId = fileId;
        Path &p = sIdsToPaths[fileId];
        if (p.isEmpty()) {
            p = path;
        } else {
            assert(p == path || path.resolved() == p);
        }
        sLastId = std::max(sLastId, fileId);
    }
private:
#ifndef RTAGS_SINGLE_THREAD
    static std::mutex sMutex;
    static void saveFileIds();
#endif
    static Hash<Path, uint32_t> sPathsToIds;
    static Hash<uint32_t, Path> sIdsToPaths;
    static uint32_t sLastId;
    enum {
        FileBits = 22,
        LineBits = 21,
        ColumnBits = 64 - FileBits - LineBits
    };
    static const uint64_t FILEID_MASK;
    static const uint64_t LINE_MASK;
    static const uint64_t COLUMN_MASK;
};

RCT_FLAGS(Location::ToStringFlag);

template <> struct FixedSize<Location>
{
    static constexpr size_t value = sizeof(Location::value);
};

template <> inline Serializer &operator<<(Serializer &s, const Location &t)
{
    s.write(reinterpret_cast<const char*>(&t.value), sizeof(uint64_t));
    return s;
}

inline bool operator==(Location loc, const String &str)
{
    const Location fromPath = Location::fromPathLineAndColumn(str);
    return loc == fromPath;
}

inline bool operator!=(Location loc, const String &str)
{
    const Location fromPath = Location::fromPathLineAndColumn(str);
    return loc != fromPath;
}

inline bool operator==(const String &str, Location loc)
{
    const Location fromPath = Location::fromPathLineAndColumn(str);
    return loc == fromPath;
}

inline bool operator!=(const String &str, Location loc)
{
    const Location fromPath = Location::fromPathLineAndColumn(str);
    return loc != fromPath;
}

template <> inline Deserializer &operator>>(Deserializer &s, Location &t)
{
    s.read(reinterpret_cast<char*>(&t), sizeof(uint64_t));
    return s;
}

static inline Log operator<<(Log dbg, Location loc)
{
    dbg << loc.toString();
    return dbg;
}

inline String Location::debug() const
{
    return toString(NoColor|AbsolutePath);
}

#endif
