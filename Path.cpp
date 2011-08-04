#include "Path.h"

Path Path::parentDir() const
{
    Path copy = *this;
    if (copy.isResolved() || copy.resolve()) {
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

Path Path::resolved(const QByteArray &path, bool *ok)
{
    Path ret(path);
    if (ret.isResolved() && ret.exists()) {
        if (ok)
            *ok = true;
        return ret;
    } else if (ret.resolve() && ok) {
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
        if (at(i) == '.' && at(++i) == '.') {
            return false;
        }
    }
    return true;
}
static QHash<QThread*, QByteArray> sThreadStorage;
static void cleanup()
{
    sThreadStorage.clear();
}

bool Path::resolve(const Path &cwd)
{
    // Q_ASSERT(!isResolved()); // probably best to avoid re-resolving
    Q_ASSERT(sThreadStorage.contains(QThread::currentThread()));
    char *buffer = sThreadStorage[QThread::currentThread()].data();
    if (!cwd.isEmpty() && !isAbsolute()) {
        Path copy = cwd + '/' + *this;
        if (copy.resolve()) {
            operator=(copy);
            return true;
        }
    }
        
    char *resolved = realpath(constData(), buffer);
    if (resolved) {
        QByteArray::operator=(resolved);
        return true;
    }
    return false;
}


void Path::initStaticData()
{
    qAddPostRoutine(cleanup);
    sThreadStorage[QThread::currentThread()] = QByteArray(PATH_MAX + 1, ' ');
}
