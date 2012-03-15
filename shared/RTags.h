#ifndef RTags_h
#define RTags_h

#include <QByteArray>
#include <clang-c/Index.h>
#include <Path.h>
#include <Log.h>
#include <stdio.h>
#include <assert.h>

namespace RTags {

enum UnitType { CompileC, CompileCPlusPlus, PchC, PchCPlusPlus };

static inline int canonicalizePath(char *path, int len)
{
    Q_ASSERT(path[0] == '/');
    for (int i=0; i<len - 3; ++i) {
        if (path[i] == '/' && path[i + 1] == '.'
            && path[i + 2] == '.' && path[i + 3] == '/') {
            for (int j=i - 1; j>=0; --j) {
                if (path[j] == '/') {
                    memmove(path + j, path + i + 3, len - (i + 2));
                    const int removed = (i + 3 - j);
                    len -= removed;
                    i -= removed;
                    break;
                }
            }
        }
    }
    return len;
}

static inline int digits(int len)
{
    int ret = 1;
    while (len >= 10) {
        len /= 10;
        ++ret;
    }
    return ret;
}

static inline QByteArray unescape(QByteArray command)
{
    command.replace('\'', "\\'");
    command.prepend("bash --norc -c 'echo -n ");
    command.append('\'');
    // QByteArray cmd = "bash --norc -c 'echo -n " + command + "'";
    FILE *f = popen(command.constData(), "r");
    QByteArray ret;
    char buf[1024];
    do {
        const int read = fread(buf, 1, 1024, f);
        if (read)
            ret += QByteArray::fromRawData(buf, read);
    } while (!feof(f));
    fclose(f);
    return ret;
}

static inline QByteArray eatString(CXString str)
{
    const QByteArray ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

static inline QByteArray join(const QList<QByteArray> &list, const QByteArray &sep = QByteArray())
{
    QByteArray ret;
    int size = qMax(0, list.size() - 1) * sep.size();
    foreach(const QByteArray &l, list) {
        size += l.size();
    }
    ret.reserve(size);
    foreach(const QByteArray &l, list) {
        ret.append(l);
    }
    return ret;
}

static inline QByteArray cursorToString(CXCursor cursor)
{
    QByteArray ret = eatString(clang_getCursorKindSpelling(clang_getCursorKind(cursor)));
    const QByteArray name = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty())
        ret += " " + name;

    CXFile file;
    unsigned line, col;
    clang_getInstantiationLocation(clang_getCursorLocation(cursor), &file, &line, &col, 0);
    const QByteArray fileName = eatString(clang_getFileName(file));
    if (!fileName.isEmpty()) {
        ret += " " + fileName + ':' + QByteArray::number(line) + ':' +  QByteArray::number(col);
    }
    return ret;
}

static inline int readLine(FILE *f, char *buf, int max)
{
    assert(!buf == (max == -1));
    if (max == -1)
        max = INT_MAX;
    for (int i=0; i<max; ++i) {
        const int ch = fgetc(f);
        if (ch == '\n' || ch == EOF) {
            if (buf)
                *buf = '\0';
            return i;
        }
        if (buf)
            *buf++ = *reinterpret_cast<const char*>(&ch);
    }
    return -1;
}

static inline QByteArray context(const Path &path, unsigned offset, unsigned col)
{
    FILE *f = fopen(path.constData(), "r");
    if (f && !fseek(f, offset - (col - 1), SEEK_SET)) {
        char buf[1024] = { '\0' };
        const int len = readLine(f, buf, 1023);
        fclose(f);
        return QByteArray(buf, len);
    }
    return QByteArray();
}

struct Location {
    Location() : line(0), column(0), offset(-1) {}

    Path path;
    int line, column, offset;
};

static inline bool makeLocation(const QByteArray &arg, Location *loc,
                                QByteArray *resolvedLocation = 0, const Path &cwd = Path())
{
    Q_ASSERT(!arg.isEmpty());
    int colon = arg.lastIndexOf(':');
    if (colon == arg.size() - 1)
        colon = arg.lastIndexOf(':', colon - 1);
    if (colon == -1) {
        return false;
    }
    const unsigned column = atoi(arg.constData() + colon + 1);
    if (!column) {
        return false;
    }
    const int colon2 = arg.lastIndexOf(':', colon - 1);
    unsigned line = 0;
    if (colon2 != -1) {
        line = atoi(arg.constData() + colon2 + 1);
        if (!line) {
            return false;
        }
        colon = colon2;
    }
    const Path path = Path::resolved(arg.left(colon), cwd);
    if (path.isEmpty()) {
        return false;
    }
    if (resolvedLocation)
        *resolvedLocation = path + arg.mid(colon);
    if (loc) {
        loc->path = path;
        if (line) {
            loc->line = line;
            loc->column = column;
            loc->offset = -1;
        } else {
            loc->offset = column;
        }
    }
    return true;
}

static inline void makeLocation(QByteArray &path, int line, int col)
{
    const int size = path.size();
    const int extra = 2 + digits(line) + digits(col);
    path.resize(size + extra);
    snprintf(path.data() + size, extra + 1, ":%d:%d", line, col);
}

enum MakeLocationFlag {
    IncludeContext = 0x1
};
static inline QByteArray makeLocation(CXCursor cursor, unsigned flags = 0)
{
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    CXFile file;
    unsigned line, col, off;
    clang_getSpellingLocation(loc, &file, &line, &col, &off);
    CXString fn = clang_getFileName(file);
    QByteArray ret;
    ret.reserve(256);
    ret += clang_getCString(fn);
    clang_disposeString(fn);
    const int len = RTags::canonicalizePath(ret.data(), ret.size());
    const int extra = RTags::digits(line) + RTags::digits(col) + 2;
    const QByteArray ctx = (flags & IncludeContext ? RTags::context(ret, off, col) : QByteArray());
    if (ctx.isEmpty()) {
        ret.resize(len + extra);
        snprintf(ret.data() + len, extra + 1, ":%d:%d", line, col);
    } else {
        ret.resize(len + extra + 1 + ctx.size());
        snprintf(ret.data() + len, extra + 1 + ctx.size(), ":%d:%d\t%s", line, col, ctx.constData());
    }
    return ret;
}

static inline QByteArray makeLocation(const QByteArray &encodedLocation)
{
    Location loc;
    QByteArray file;
    if (makeLocation(encodedLocation, &loc, &file)) {
        const QByteArray ctx = RTags::context(file, loc.line, loc.column);
        return encodedLocation + '\t' + ctx;
    }
    return encodedLocation;
}
}

#endif
