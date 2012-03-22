#ifndef RTags_h
#define RTags_h

#include <QByteArray>
#include <Path.h>
#include <Log.h>
#include <stdio.h>
#include <assert.h>
#include <getopt.h>

namespace RTags {
enum { DatabaseVersion = 2 };
enum UnitType { CompileC, CompileCPlusPlus, PchC, PchCPlusPlus };

QByteArray shortOptions(const option *longOptions);
int readLine(FILE *f, char *buf, int max);
static inline int digits(int len)
{
    int ret = 1;
    while (len >= 10) {
        len /= 10;
        ++ret;
    }
    return ret;
}

struct Location {
    Location(const Path &p = Path(), int off = 0)
        : path(p), offset(off)
    {}

    Path path;
    int offset;

    bool isNull() const
    {
        return !offset;
    }

    inline bool operator==(const Location &other) const
    {
        return path == other.path && offset == other.offset;
    }
    inline bool operator!=(const Location &other) const
    {
        return offset != other.offset || path != other.path;
    }
    
    inline bool operator<(const Location &other) const
    {
        const int cmp = strcmp(path.constData(), other.path.constData());
        if (cmp < 0) {
            return true;
        } else if (cmp > 0) {
            return false;
        }
        return offset < other.offset;
    }

    enum KeyFlag {
        NoFlag = 0x0,
        Padded = 0x1,
        ShowContext = 0x2
    };
    QByteArray context() const
    {
        unsigned off = offset - 1;// offset is 1-indexed, files are not
        FILE *f = fopen(path.constData(), "r");
        if (f && !fseek(f, off, SEEK_SET)) {
            while (off > 0) {
                char ch = fgetc(f);
                if (ch == '\n')
                    break;
                if (fseek(f, --off, SEEK_SET) == -1) {
                    fclose(f);
                    return QByteArray();
                }
            }
            char buf[1024] = { '\0' };
            const int len = readLine(f, buf, 1023);
            fclose(f);
            return QByteArray(buf, len);
        }
        if (f)
            fclose(f);
        return QByteArray();
    }

    QByteArray key(unsigned flags = NoFlag) const
    {
        if (!offset)
            return QByteArray();
        int extra = flags & Padded ? 7 : RTags::digits(offset) + 1;
        QByteArray ctx;
        if (flags & ShowContext) {
            ctx += '\t' + context();
            extra += ctx.size();
        }

        QByteArray ret(path.size() + extra, '0');
        memcpy(ret.data(), path.constData(), path.size());

        if (flags & Padded) {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%06d%s", path.constData(),
                     offset, ctx.constData());
        } else {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%d%s", path.constData(),
                     offset, ctx.constData());
        }
        return ret;
    }

    static RTags::Location fromKey(const QByteArray &key)
    {
        if (key.isEmpty())
            return Location();
        const int lastComma = key.lastIndexOf(',');
        Q_ASSERT(lastComma > 0 && lastComma + 1 < key.size());
        Location ret;
        ret.offset = atoi(key.constData() + lastComma + 1);
        Q_ASSERT(ret.offset);
        ret.path = key.left(lastComma);
        return ret;
    }
};

static inline QDataStream &operator<<(QDataStream &ds, const Location &loc)
{
    ds << loc.path << loc.offset;
    return ds;
}

static inline QDataStream &operator>>(QDataStream &ds, Location &loc)
{
    ds >> loc.path >> loc.offset;
    return ds;
}

static inline QDebug operator<<(QDebug dbg, const Location &loc)
{
    const QByteArray out = "Location(" + loc.key() + ")";
    return (dbg << out);
}

static inline uint qHash(const Location &l)
{
    // ### this should be done better
    return qHash(l.path) + l.offset;
}

int canonicalizePath(char *path, int len);
QByteArray unescape(QByteArray command);
QByteArray join(const QList<QByteArray> &list, const QByteArray &sep = QByteArray());

bool makeLocation(const QByteArray &arg, Location *loc,
                  QByteArray *resolvedLocation = 0, const Path &cwd = Path());
}

#endif
