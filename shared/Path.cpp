#include "Path.h"
#include <QCoreApplication>
#include <QThread>
#include <stdio.h>
#include "RTags.h"

// this doesn't check if *this actually is a real file
Path Path::parentDir() const
{
    Path copy = *this;
    int i = copy.size() - 1;
    while (copy.at(i) == '/')
        --i;
    while (i >= 0 && copy.at(i) != '/')
        --i;
    if (i < 0)
        return Path();
    copy.truncate(i + 1);
    Q_ASSERT(copy.endsWith('/'));
    return copy;
}

Path::Type Path::type() const
{
    struct stat st;
    if (stat(constData(), &st) == -1)
        return Invalid;

    switch (st.st_mode & S_IFMT) {
    case S_IFBLK: return BlockDevice;
    case S_IFCHR: return CharacterDevice;
    case S_IFDIR: return Directory;
    case S_IFIFO: return NamedPipe;
    case S_IFLNK: return SymLink;
    case S_IFREG: return File;
    case S_IFSOCK: return Socket;
    default:
        break;
    }
    return Invalid;
}

time_t Path::lastModified() const
{
    struct stat st;
    if (stat(constData(), &st) == -1) {
        qWarning("Stat failed for %s", constData());
        return 0;
    }
    return st.st_mtime;
}

int64_t Path::fileSize() const
{
    struct stat st;
    if (!stat(constData(), &st) && st.st_mode == S_IFREG)
        return st.st_size;
    return -1;
}

Path Path::resolved(const QByteArray &path, const Path &cwd, bool *ok)
{
    Path ret(path);
    if (ret.isResolved() && ret.exists()) {
        if (ok)
            *ok = true;
        return ret;
    } else if (ret.resolve(cwd) && ok) {
        *ok = true;
    } else if (ok) {
        *ok = false;
    }
    return ret;
}

bool Path::isResolved() const
{
    return isAbsolute() && isCanonical();
}

bool Path::isCanonical() const
{
    return lastIndexOf("/../") == -1;
}

int Path::canonicalize()
{
    const int s = size();
    const int ret = RTags::canonicalizePath(data(), s);
    if (s != ret)
        truncate(ret);
    return ret;
}

bool Path::resolve(const Path &cwd)
{
    // Q_ASSERT(!isResolved()); // probably best to avoid re-resolving
    if (!cwd.isEmpty() && !isAbsolute()) {
        Path copy = cwd + '/' + *this;
        if (copy.resolve()) {
            operator=(copy);
            return true;
        }
    }

    {
        char buffer[PATH_MAX + 1];
        char *resolved = realpath(constData(), buffer);
        if (resolved) {
            QByteArray::operator=(resolved);
            return true;
        }
    }
    return false;
}

const char * Path::fileName() const
{
    return constData() + lastIndexOf('/') + 1;
}

const char * Path::extension() const
{
    const int dot = lastIndexOf('.');
    if (dot == -1 || dot + 1 == size())
        return 0;
    return constData() + dot + 1;
}

bool Path::isSource(const char *ext, int len)
{
    const char *sources[] = { "c", "cpp", "cxx", "cc", "moc", 0 };
    for (int i=0; sources[i]; ++i) {
        if (!strncasecmp(ext, sources[i], len))
            return true;
    }
    return false;
}

bool Path::isSource() const
{
    if (exists()) {
        const char *ext = extension();
        if (ext)
            return isSource(ext, strlen(ext));
    }
    return false;
}

bool Path::isHeader() const
{
    if (exists()) {
        const char *ext = extension();
        if (ext) {
            const char *headers[] = { "h", "hpp", "hxx", "hh", "tcc", 0 };
            const int len = strlen(ext);
            for (int i=0; headers[i]; ++i) {
                if (!strncasecmp(ext, headers[i], len))
                    return true;
            }
        }
    }
    return false;
}

bool Path::isSystem(const char *path)
{
    if (!strncmp("/usr/", path, 5)) {
#ifdef Q_OS_BSD4
        if (!strncmp("home/", path + 5, 5))
            return false;
#endif
        return true;
    }
    return false;
}

Path Path::canonicalized(const QByteArray &path)
{
    Path p(path);
    p.canonicalize();
    return p;
}
