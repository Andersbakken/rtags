#ifndef Location_h
#define Location_h

#include <QByteArray>
#include <QReadWriteLock>
#include <Path.h>
#include <Log.h>
#include <stdio.h>
#include <assert.h>
#include "RTags.h"

class Location
{
public:
    Location()
        : mData(0)
    {}
    Location(quint32 fileId, quint32 offset)
        : mData(quint64(fileId) << 32 | offset)
    {}
    static inline quint32 insertFile(const Path &path)
    {
        QWriteLocker lock(&sLock);
        quint32 &id = sPathsToIds[path];
        if (!id) {
            id = ++sLastId;
            sIdsToPaths[id] = path;
        }

        return id;
    }
    static void init(const QHash<Path, quint32> &pathsToIds,
                     const QHash<quint32, Path> &idsToPaths)
    {
        sPathsToIds = pathsToIds;
        sIdsToPaths = idsToPaths;
    }

    inline quint32 offset() const { return quint32(mData >> 32); }
    inline quint32 fileId() const { return quint32(mData); }

    inline Path path() const
    {
        QReadLocker lock(&sLock);
        return sIdsToPaths.value(fileId());
    }
    inline bool isNull() const { return !mData; }
    inline void clear() { mData = 0; }
    inline bool operator==(const Location &other) const { return mData == other.mData; }
    inline bool operator!=(const Location &other) const { return mData != other.mData; }
    inline bool operator<(const Location &other) const { return mData < other.mData; }

    enum KeyFlag {
        NoFlag = 0x0,
        Padded = 0x1,
        ShowContext = 0x2,
        ShowLineNumbers = 0x4
    };
    QByteArray context() const
    {
        const quint32 off = offset();
        quint32 o = off;
        Path p = path();
        FILE *f = fopen(p.constData(), "r");
        if (f && !fseek(f, off, SEEK_SET)) {
            while (o > 0) {
                const char ch = fgetc(f);
                if (ch == '\n' && o != off)
                    break;
                if (fseek(f, --o, SEEK_SET) == -1) {
                    fclose(f);
                    return QByteArray();
                }
            }
            char buf[1024] = { '\0' };
            const int len = RTags::readLine(f, buf, 1023);
            fclose(f);
            return QByteArray(buf, len);
        }
        if (f)
            fclose(f);
        return QByteArray();
    }

    bool convertOffset(int &line, int &col) const
    {
        const quint32 off = offset();
        Path p = path();
        FILE *f = fopen(p.constData(), "r");
        if (!f) {
            line = col = -1;
            return false;
        }
        line = 1;
        int last = 0;
        quint32 idx = 0;
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


    QByteArray key(unsigned flags = NoFlag) const
    {
        if (isNull())
            return QByteArray();
        int extra = 0;
        const int off = offset();
        int line = 0, col = 0;
        if (flags & Padded) {
            extra = 7;
        } else if (flags & ShowLineNumbers && convertOffset(line, col)) {
            extra = RTags::digits(line) + RTags::digits(col) + 3;
        } else {
            flags &= ~ShowLineNumbers;
            extra = RTags::digits(off) + 1;
        }
        QByteArray ctx;
        if (flags & ShowContext) {
            ctx += '\t' + context();
            extra += ctx.size();
        }

        const Path p = path();

        QByteArray ret(p.size() + extra, '0');

        if (flags & Padded) {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%06d%s", p.constData(),
                     off, ctx.constData());
        } else if (flags & ShowLineNumbers) {
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
            memset(buf, 0, sizeof(buf));
            return false;
        } else {
            Q_ASSERT(sizeof(buf) == sizeof(mData));
            memcpy(buf, &mData, sizeof(buf));
            return true;
        }
    }

    enum FromKeyFlag {
        None = 0x0,
        Resolve = 0x1,
        Canonicalize = 0x2
    };
    // static Location fromKey(const QByteArray &key, unsigned flags = 0, const Path &cwd = Path())
    // {
    //     const int lastComma = key.lastIndexOf(',');
    //     if (lastComma <= 0 || lastComma + 1 >= key.size())
    //         return Location();
    //     Location ret;
    //     char *endPtr;
    //     ret.offset = strtoull(key.constData() + lastComma + 1, &endPtr, 10);
    //     if (*endPtr != '\0')
    //         return Location();
    //     ret.path = key.left(lastComma);
    //     if (flags == Canonicalize) {
    //         ret.path.canonicalize();
    //     } else if (flags & Resolve) {
    //         ret.path.resolve(cwd);
    //     }
    //     return ret;
    // }
    static Location fromKey(const char *data)
    {
        Location ret;
        memcpy(&ret.mData, data, sizeof(mData));
        return ret;
    }

    static Location decodeClientLocation(const QByteArray &data)
    {
        QDataStream ds(data);
        Path path;
        quint32 offset;
        ds >> path >> offset;
        QReadLocker lock(&sLock);
        const quint32 fileId = sPathsToIds.value(path, 0);
        if (fileId)
            return Location(fileId, offset);
        return Location();
    }
    quint64 mData;
private:
    static QHash<Path, quint32> sPathsToIds;
    static QHash<quint32, Path> sIdsToPaths;
    static quint32 sLastId;
    static QReadWriteLock sLock;
};

static inline QDataStream &operator<<(QDataStream &ds, const Location &loc)
{
    ds << loc.mData;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Location &loc)
{
    ds >> loc.mData;
    return ds;
}

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    const QByteArray out = "Location(" + loc.key() + ")";
    return (dbg << out);
}

static inline uint qHash(const Location &l)
{
    return qHash(l.mData);
}

#endif
