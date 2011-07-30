#include "Path.h"

Path Path::parentDir() const
{
    Path copy = *this;
    if (copy.resolve()) {
        if (isDir()) {
            int i = copy.size() - 1;
            while (copy.at(i) == '/')
                --i;
            while (i > 0 && copy.at(i) != '/')
                --i;
            if (i > 0)
                copy.truncate(i);
        } else {
            Q_ASSERT(!copy.endsWith('/'));
            for (int i=copy.size() - 1; i>=0; --i) {
                if (copy.at(i) == '/') {
                    copy.truncate(i);
                    break;
                }
            }
        }
    }
    if (copy.isDir()) {
        return copy;
    } else {
        return Path();
    }
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
    if (stat(constData(), &st) == -1)
        return 0;
    return st.st_mtime;
}

qint64 Path::fileSize() const
{
    struct stat st;
    if (!stat(constData(), &st) && st.st_mode == S_IFREG)
        return st.st_size;
    return -1;
}

bool Path::resolve()
{
    bool resolve = !isAbsolute();
    if (!resolve) {
        const int count = size();
        for (int i=1; i<count - 1; ++i) {
            if (at(i) == '.' && at(++i) == '.') {
                resolve = true;
                break;
            }
        }
    }

    if (!resolve)
        return exists();

    // ### consider using thread-local static buffer of PATH_MAX
    char *resolved = realpath(constData(), 0);
    if (resolved) {
        QByteArray::operator=(resolved);
        return true;
    }
    return false;
}

Path Path::resolved(const QByteArray &path, bool *ok)
{
    Path ret(path);
    if (ret.resolve() && ok) {
        *ok = true;
    } else if (ok) {
        *ok = false;
    }
    return ret;
}

