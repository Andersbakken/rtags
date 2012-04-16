#ifndef RTags_h
#define RTags_h

#include <QByteArray>
#include <Path.h>
#include <Log.h>
#include <stdio.h>
#include <assert.h>
#include <getopt.h>

namespace RTags {
enum UnitType { CompileC, CompileCPlusPlus, PchC, PchCPlusPlus };

QByteArray shortOptions(const option *longOptions);
int readLine(FILE *f, char *buf = 0, int max = -1);
int removeDirectory(const char *path);

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
    Location(const Path &p = Path(), int off = -1)
        : path(p), offset(off)
    {}

    Path path;
    int offset;

    bool isNull() const
    {
        return (offset == -1);
    }

    void clear()
    {
        path.clear();
        offset = -1;
    }

    inline bool operator==(const Location &other) const
    {
        return offset == other.offset && path == other.path;
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
        ShowContext = 0x2,
        LineNumbers = 0x4
    };
    QByteArray context() const
    {
        int off = offset;
        FILE *f = fopen(path.constData(), "r");
        if (f && !fseek(f, off, SEEK_SET)) {
            while (off > 0) {
                const char ch = fgetc(f);
                if (ch == '\n' && off != offset)
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

    bool convertOffset(int &line, int &col) const
    {
        FILE *f = fopen(path.constData(), "r");
        if (!f) {
            line = col = -1;
            return false;
        }
        line = 1;
        int last = 0;
        int idx = 0;
        forever {
            const int lineLen = readLine(f);
            if (lineLen == -1) {
                col = line = -1;
                fclose(f);
                return false;
            }
            idx += lineLen + 1;
            // printf("lineStart %d offset %d last %d lineLen %d\n", idx, offset, last, lineLen);
            if (idx > offset) {
                col = offset - last;
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
        if (offset == -1)
            return QByteArray();
        int extra = 0;
        int line = 0, col = 0;
        if (flags & Padded) {
            extra = 7;
        } else if (flags & LineNumbers && convertOffset(line, col)) {
            extra = RTags::digits(line) + RTags::digits(col) + 2;
        } else {
            flags &= ~LineNumbers;
            extra = RTags::digits(offset) + 1;
        }
        QByteArray ctx;
        if (flags & ShowContext) {
            ctx += '\t' + context();
            extra += ctx.size();
        }

        QByteArray ret(path.size() + extra, '0');

        if (flags & Padded) {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%06d%s", path.constData(),
                     offset, ctx.constData());
        } else if (flags & LineNumbers) {
            snprintf(ret.data(), ret.size() + extra + 1, "%s:%d:%d%s %d", path.constData(),
                     line, col, ctx.constData(), offset);
        } else {
            snprintf(ret.data(), ret.size() + extra + 1, "%s,%d%s", path.constData(),
                     offset, ctx.constData());
        }
        return ret;
    }

    static RTags::Location fromKey(const QByteArray &key)
    {
        const int lastComma = key.lastIndexOf(',');
        if (lastComma <= 0 || lastComma + 1 >= key.size())
            return Location();
        Location ret;
        char *endPtr;
        ret.offset = strtoull(key.constData() + lastComma + 1, &endPtr, 10);
        if (*endPtr != '\0')
            return Location();
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
