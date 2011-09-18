#include "Path.h"

// this doesn't check if *this actually is a real file
Path Path::parentDir() const
{
    Path copy = *this;
    int i = copy.size() - 1;
    while (copy.at(i) == '/')
        --i;
    while (i > 0 && copy.at(i) != '/')
        --i;
    if (i > 0)
        copy.truncate(i);
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
    if (!isAbsolute())
        return false;
    const int count = size();
    for (int i=1; i<count - 1; ++i) {
        if (at(i) == '.') {
            switch (at(i + 1)) {
            case '.':
                return false;
            case '/':
                if (at(i - 1) == '/')
                    return false;
            default:
                break;
            }
            ++i;
        }
    }
    return true;
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

bool Path::isSource() const
{
    if (isFile()) {
        const int lastDot = lastIndexOf('.');
        const int len = size() - lastDot;
        if (lastDot != -1 && len > 0) {
            const char *sourceFileExtensions[] = {
                "c", "h", "cpp", "hpp", "cxx", "hxx", "moc", "cc", "hh", 0
            };
            const char *str = constData() + lastDot + 1;
            for (int i=0; sourceFileExtensions[i]; ++i) {
                if (!strncasecmp(str, sourceFileExtensions[i], len)) {
                    return true;
                }
            }
        }
    }
    return false;
}
