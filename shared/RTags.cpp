#include "RTags.h"
#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>

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

ByteArray unescape(ByteArray command)
{
    command.replace("\'", "\\'");
    command.prepend("bash --norc -c 'echo -n ");
    command.append('\'');
    // ByteArray cmd = "bash --norc -c 'echo -n " + command + "'";
    FILE *f = popen(command.constData(), "r");
    ByteArray ret;
    char buf[1024];
    do {
        const int read = fread(buf, 1, 1024, f);
        if (read)
            ret += ByteArray(buf, read);
    } while (!feof(f));
    fclose(f);
    return ret;
}


ByteArray join(const QList<ByteArray> &list, const ByteArray &sep)
{
    ByteArray ret;
    int size = qMax(0, list.size() - 1) * sep.size();
    foreach (const ByteArray &l, list) {
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
        switch (ch) {
        case EOF:
            if (!i)
                i = -1;
            // fall through
        case '\n':
            if (buf)
                *buf = '\0';
            return i;
        }
        if (buf)
            *buf++ = *reinterpret_cast<const char*>(&ch);
    }
    return -1;
}


ByteArray shortOptions(const option *longOptions)
{
    ByteArray ret;
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

bool removeDirectory(const char *path)
{
    DIR *d = opendir(path);
    size_t path_len = strlen(path);
    int r = -1;

    if (d) {
        struct dirent *p;

        r = 0;

        while (!r && (p=readdir(d))) {
            int r2 = -1;
            char *buf;
            size_t len;

            /* Skip the names "." and ".." as we don't want to recurse on them. */
            if (!strcmp(p->d_name, ".") || !strcmp(p->d_name, "..")) {
                continue;
            }

            len = path_len + strlen(p->d_name) + 2;
            buf = static_cast<char*>(malloc(len));

            if (buf) {
                struct stat statbuf;
                snprintf(buf, len, "%s/%s", path, p->d_name);
                if (!stat(buf, &statbuf)) {
                    if (S_ISDIR(statbuf.st_mode)) {
                        r2 = removeDirectory(buf);
                    } else {
                        r2 = unlink(buf);
                    }
                }

                free(buf);
            }

            r = r2;
        }

        closedir(d);
    }

    if (!r) {
        r = rmdir(path);
    }

    return !r;
}
bool startProcess(const Path &dotexe, const QList<ByteArray> &dollarArgs)
{
    switch (fork()) {
    case 0:
        break;
    case -1:
        return false;
    default:
        return true;
    }

    if (setsid() < 0)
        _exit(1);


    switch (fork()) {
    case 0:
        break;
    case -1:
        _exit(1);
    default:
        _exit(0);
    }

    int ret = chdir("/");
    if (ret == -1)
        perror("RTags::startProcess() Failed to chdir(\"/\")");

    umask(0);

    const int fdlimit = sysconf(_SC_OPEN_MAX);
    for (int i=0; i<fdlimit; ++i)
        close(i);

    open("/dev/null", O_RDWR);
    ret = dup(0);
    if (ret == -1)
        perror("RTags::startProcess() Failed to duplicate fd");
    ret = dup(0);
    if (ret == -1)
        perror("RTags::startProcess() Failed to duplicate fd");
    char **args = new char*[dollarArgs.size() + 2];
    args[0] = strndup(dotexe.constData(), dotexe.size());
    for (int i=0; i<dollarArgs.size(); ++i) {
        args[i + 1] = strndup(dollarArgs.at(i).constData(), dollarArgs.at(i).size());
    }
    args[dollarArgs.size() + 1] = 0;
    execvp(dotexe.constData(), args);
    FILE *f = fopen("/tmp/failedtolaunch", "w");
    if (f) {
        fwrite(dotexe.constData(), 1, dotexe.size(), f);
        fwrite(" ", 1, 1, f);
        const ByteArray joined = RTags::join(dollarArgs, " ");
        fwrite(joined.constData(), 1, joined.size(), f);
        fclose(f);
    }
    _exit(1);
    return false;
}
}


