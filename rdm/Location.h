#ifndef Location_h
#define Location_h

#include <ByteArray.h>
#include <QReadWriteLock>
#include <Path.h>
#include <Log.h>
#include <stdio.h>
#include <assert.h>
#include "RTags.h"
#include <clang-c/Index.h>
#include <Serializer.h>

class Location
{
public:
    Location()
        : mData(0)
    {}
    Location(uint64_t data)
        : mData(data)
    {}
    Location(uint32_t fileId, uint32_t offset)
        : mData(uint64_t(offset) << 32 | fileId)
    {}

    Location(const CXFile &file, uint32_t offset)
        : mData(0)
    {
        Q_ASSERT(file);
        CXString fn = clang_getFileName(file);
        const char *cstr = clang_getCString(fn);
        if (!cstr)
            return;
        const Path p = Path::canonicalized(cstr);
        clang_disposeString(fn);
        uint32_t fileId = insertFile(p);
        mData = (uint64_t(offset) << 32) | fileId;
    }
    Location(const CXSourceLocation &location)
        : mData(0)
    {
        CXFile file;
        unsigned offset;
        clang_getSpellingLocation(location, &file, 0, 0, &offset);
        *this = Location(file, offset);
    }
    static inline uint32_t fileId(const Path &path)
    {
        QReadLocker lock(&sLock);
        return sPathsToIds.value(path);
    }
    static inline Path path(uint32_t id)
    {
        QReadLocker lock(&sLock);
        return sIdsToPaths.value(id);
    }

    static inline uint32_t insertFile(const Path &path)
    {
        bool newFile = false;
        uint32_t ret;
        {
            QWriteLocker lock(&sLock);
            uint32_t &id = sPathsToIds[path];
            if (!id) {
                id = ++sLastId;
                sIdsToPaths[id] = path;
                newFile = true;
            }
            ret = id;
        }
        if (newFile)
            writeToDB(path, ret);

        return ret;
    }
    static void writeToDB(const Path &path, uint32_t file);
    static void init(const Map<Path, uint32_t> &pathsToIds,
                     const Map<uint32_t, Path> &idsToPaths,
                     uint32_t maxId)
    {
        sPathsToIds = pathsToIds;
        sIdsToPaths = idsToPaths;
        sLastId = maxId;
    }

    inline uint32_t fileId() const { return uint32_t(mData); }
    inline uint32_t offset() const { return uint32_t(mData >> 32); }

    inline Path path() const
    {
        if (mCachedPath.isEmpty()) {
            QReadLocker lock(&sLock);
            mCachedPath = sIdsToPaths.value(fileId());
        }
        return mCachedPath;
    }
    inline bool isNull() const { return !mData; }
    inline bool isValid() const { return mData; }
    inline void clear() { mData = 0; mCachedPath.clear(); }
    inline bool operator==(const Location &other) const { return mData == other.mData; }
    inline bool operator!=(const Location &other) const { return mData != other.mData; }
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

    ByteArray context() const
    {
        const uint32_t off = offset();
        uint32_t o = off;
        Path p = path();
        FILE *f = fopen(p.constData(), "r");
        if (f && !fseek(f, off, SEEK_SET)) {
            while (o > 0) {
                const char ch = fgetc(f);
                if (ch == '\n' && o != off)
                    break;
                if (fseek(f, --o, SEEK_SET) == -1) {
                    fclose(f);
                    return ByteArray();
                }
            }
            char buf[1024] = { '\0' };
            const int len = RTags::readLine(f, buf, 1023);
            fclose(f);
            return ByteArray(buf, len);
        }
        if (f)
            fclose(f);
        return ByteArray();
    }

    bool convertOffset(int &line, int &col) const
    {
        const uint32_t off = offset();
        Path p = path();
        FILE *f = fopen(p.constData(), "r");
        if (!f) {
            line = col = -1;
            return false;
        }
        line = 1;
        int last = 0;
        uint32_t idx = 0;
        forever {
            const int lineLen = RTags::readLine(f);
            if (lineLen == -1) {
                col = line = -1;
                fclose(f);
                return false;
            }
            idx += lineLen + 1;
            // printf("lineStart %d offset %d last %d lineLen %d\n", idx, offset, last, lineLen);
            if (idx > off) {
                col = off - last + 1;
                break;
            }
            last = idx;
            ++line;
        }
        fclose(f);
        return true;
    }


    ByteArray key(unsigned flags = RTags::NoFlag) const
    {
        if (isNull())
            return ByteArray();
        int extra = 0;
        const int off = offset();
        int line = 0, col = 0;
        if (flags & RTags::Padded) {
            extra = 7;
        } else if (flags & RTags::ShowLineNumbers && convertOffset(line, col)) {
            extra = RTags::digits(line) + RTags::digits(col) + 3;
        } else {
            flags &= ~RTags::ShowLineNumbers;
            extra = RTags::digits(off) + 1;
        }
        ByteArray ctx;
        if (flags & RTags::ShowContext) {
            ctx += '\t';
            ctx += context();
            extra += ctx.size();
        }

        const Path p = path();

        ByteArray ret(p.size() + extra, '0');

        if (flags & RTags::Padded) {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%06d%s", p.constData(),
                     off, ctx.constData());
        } else if (flags & RTags::ShowLineNumbers) {
            snprintf(ret.data(), ret.size() + extra + 1, "%s:%d:%d:%s", p.constData(),
                     line, col, ctx.constData());
        } else {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%d%s", p.constData(),
                     off, ctx.constData());
        }
        return ret;
    }

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

    static Location decodeClientLocation(const ByteArray &data)
    {
        uint32_t offset;
        memcpy(&offset, data.constData() + data.size() - sizeof(offset), sizeof(offset));
        const Path path(data.constData(), data.size() - sizeof(offset));
        QReadLocker lock(&sLock);
        const uint32_t fileId = sPathsToIds.value(path, 0);
        if (fileId)
            return Location(fileId, offset);
        error("Failed to make location from [%s,%d]", path.constData(), offset);
        return Location();
    }
    static Location fromPathAndOffset(const ByteArray &pathAndOffset)
    {
        const int comma = pathAndOffset.lastIndexOf(',');
        if (comma <= 0 || comma + 1 == pathAndOffset.size()) {
            error("Can't create location from this: %s", pathAndOffset.constData());
            return Location();
        }
        bool ok;
        const uint32_t fileId = ByteArray(pathAndOffset.constData() + comma + 1, pathAndOffset.size() - comma - 1).toUInt(&ok);
        if (!ok) {
            error("Can't create location from this: %s", pathAndOffset.constData());
            return Location();
        }
        return Location(Location::insertFile(Path(pathAndOffset.left(comma))), fileId);
    }
    uint64_t mData;
private:
    static Map<Path, uint32_t> sPathsToIds;
    static Map<uint32_t, Path> sIdsToPaths;
    static uint32_t sLastId;
    static QReadWriteLock sLock;
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

static inline Log &operator<<(Log &dbg, const Location &loc)
{
    const ByteArray out = "Location(" + loc.key() + ")";
    return (dbg << out);
}

#endif
