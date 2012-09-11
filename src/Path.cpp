#include "Path.h"
#include <stdio.h>
#include "RTags.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

// this doesn't check if *this actually is a real file
Path Path::parentDir() const
{
    if (isEmpty())
        return Path();
    Path copy = *this;
    int i = copy.size() - 1;
    while (copy.at(i) == '/')
        --i;
    while (i >= 0 && copy.at(i) != '/')
        --i;
    if (i < 0)
        return Path();
    copy.truncate(i + 1);
    assert(copy.endsWith('/'));
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
        warning("Stat failed for %s", constData());
        return 0;
    }
    return st.st_mtime;
}

int64_t Path::fileSize() const
{
    struct stat st;
    if (!stat(constData(), &st)) {// && st.st_mode == S_IFREG)
        return st.st_size;
    }
    return -1;
}

Path Path::resolved(const ByteArray &path, const Path &cwd, bool *ok)
{
    Path ret(path);
    if (ret.resolve(cwd) && ok) {
        *ok = true;
    } else if (ok) {
        *ok = false;
    }
    return ret;
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
    if (!cwd.isEmpty() && !isAbsolute()) {
        Path copy = cwd + '/' + *this;
        if (copy.resolve()) {
            operator=(copy);
            return true;
        }
    }

    {
        char buffer[PATH_MAX + 1];
        if (realpath(constData(), buffer)) {
            ByteArray::operator=(buffer);
            return true;
        }
    }
    return false;
}

const char * Path::fileName(int *len) const
{
    const int idx = lastIndexOf('/') + 1;
    if (len)
        *len = size() - idx;
    return constData() + idx;
}

const char * Path::extension() const
{
    const int dot = lastIndexOf('.');
    if (dot == -1 || dot + 1 == size())
        return 0;
    return constData() + dot + 1;
}

bool Path::isSource(const char *ext)
{
    const char *sources[] = { "c", "cc", "cpp", "cxx", "moc", 0 };
    for (int i=0; sources[i]; ++i) {
        if (!strcasecmp(ext, sources[i]))
            return true;
    }
    return false;
}

bool Path::isSource() const
{
    if (exists()) {
        const char *ext = extension();
        if (ext)
            return isSource(ext);
    }
    return false;
}

bool Path::isHeader() const
{
    if (exists()) {
        const char *ext = extension();
        if (ext)
            return isHeader(ext);
    }
    return false;
}

bool Path::isHeader(const char *ext)
{
    const char *headers[] = { "h", "hpp", "hxx", "hh", "tcc", 0 };
    for (int i=0; headers[i]; ++i) {
        if (!strcasecmp(ext, headers[i]))
            return true;
    }
    return false;
}

bool Path::isSystem(const char *path)
{
    if (!strncmp("/usr/", path, 5)) {
#ifdef OS_FreeBSD
        if (!strncmp("home/", path + 5, 5))
            return false;
#endif
        return true;
    }
    return false;
}

Path Path::canonicalized(const ByteArray &path)
{
    Path p(path);
    p.canonicalize();
    return p;
}

bool Path::mksubdir(const ByteArray &path) const
{
    if (isDir()) {
        ByteArray combined = *this;
        if (!combined.endsWith('/'))
            combined.append('/');
        combined.append(path);
        return Path::mkdir(combined);
    }
    return false;
}

bool Path::mkdir(const Path &path)
{
    const mode_t mode = S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH;
    errno = 0;
    return !::mkdir(path.constData(), mode) || errno == EEXIST;
}
bool Path::rm(const Path &file)
{
    return !unlink(file.constData());
}

void Path::visit(VisitCallback callback, void *userData) const
{
    if (!callback)
        return;
    DIR *d = opendir(constData());
    if (!d)
        return;
    char buf[PATH_MAX + sizeof(dirent) + 1];
    dirent *dbuf = reinterpret_cast<dirent*>(buf);

    dirent *p;
    Path path = *this;
    if (!path.endsWith('/'))
        path.append('/');
    const int s = path.size();
    path.reserve(s + 128);
    List<ByteArray> recurseDirs;
    while (!readdir_r(d, dbuf, &p) && p) {
        if (!strcmp(p->d_name, ".") || !strcmp(p->d_name, ".."))
            continue;
        path.truncate(s);
        path.append(p->d_name);
#if defined(_DIRENT_HAVE_D_TYPE) && defined(_BSD_SOURCE)
        if (p->d_type == DT_DIR)
            path.append('/');
#else
        if (path.isDir())
            path.append('/');
#endif
        switch (callback(path, userData)) {
        case Abort:
            p = 0;
            break;
        case Recurse:
            if (path.isDir())
                recurseDirs.append(p->d_name);
            break;
        case Continue:
            break;
        }
    }
    closedir(d);
    const int count = recurseDirs.size();
    for (int i=0; i<count; ++i) {
        path.truncate(s);
        path.append(recurseDirs.at(i));
        path.visit(callback, userData);
    }
}

Path Path::followLink(bool *ok) const
{
    if (isSymLink()) {
        char buf[PATH_MAX];
        int w = readlink(constData(), buf, sizeof(buf) - 1);
        if (w != -1) {
            buf[w] = '\0';
            return buf;
        }
    }
    return *this;
}

int Path::readAll(char *&buf, int max) const
{
    FILE *f = fopen(constData(), "r");
    buf = 0;
    if (!f)
        return -1;
    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    if (max > 0 && max < size)
        size = max;
    if (size) {
        fseek(f, 0, SEEK_SET);
        buf = new char[size + 1];
        const int ret = fread(buf, sizeof(char), size, f);
        if (ret != size) {
            size = -1;
            delete[] buf;
        }
        buf[size] = '\0';
    }
    fclose(f);
    return size;
}

Path Path::home()
{
    Path ret = Path::resolved(getenv("HOME"));
    if (!ret.endsWith('/'))
        ret.append('/');
    return ret;
}
