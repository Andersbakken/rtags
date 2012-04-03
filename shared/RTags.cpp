#include "RTags.h"

namespace RTags {

int canonicalizePath(char *path, int len)
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

QByteArray unescape(QByteArray command)
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


QByteArray join(const QList<QByteArray> &list, const QByteArray &sep)
{
    QByteArray ret;
    int size = qMax(0, list.size() - 1) * sep.size();
    foreach (const QByteArray &l, list) {
        size += l.size();
    }
    ret.reserve(size);
    for (int i=0; i<list.size(); ++i) {
        ret.append(list.at(i));
        if (i + 1 < list.size()) {
            ret.append(sep);
        }
    }
    return ret;
}

int readLine(FILE *f, char *buf, int max)
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


bool makeLocation(const QByteArray &arg, Location *loc,
                  QByteArray *resolvedLocation, const Path &cwd)
{
    Location l = Location::fromKey(arg);
    if (!l.offset) {
        return false;
    }
    if (!l.path.resolve(cwd))
        return false;
    if (resolvedLocation) {
        const int comma = arg.lastIndexOf(',');
        *resolvedLocation = l.path + arg.mid(comma);
    }
    if (loc)
        *loc = l;
    return true;
}

QByteArray shortOptions(const option *longOptions)
{
    QByteArray ret;
    for (int i=0; longOptions[i].name; ++i) {
        Q_ASSERT(!ret.contains(longOptions[i].val));
        ret.append(longOptions[i].val);
        switch (longOptions[i].has_arg) {
        case no_argument:
            break;
        case optional_argument:
            ret.append(':');
            ret.append(':');
            break;
        case required_argument:
            ret.append(':');
            break;
        default:
            Q_ASSERT(0);
            break;
        }
    }
    return ret;
}


}

