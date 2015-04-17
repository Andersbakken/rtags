/* This file is part of RTags (http://rtags.net).

RTags is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

RTags is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "RTags.h"
#include "Server.h"
#include "VisitFileMessage.h"
#include "VisitFileResponseMessage.h"
#include "IndexDataMessage.h"
#include "IndexMessage.h"
#include "LogOutputMessage.h"
#include "QueryMessage.h"
#include <dirent.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <rct/Rct.h>
#include <rct/StopWatch.h>
#include <sys/types.h>
#ifdef OS_FreeBSD
#include <sys/sysctl.h>
#endif
#ifdef OS_Darwin
#include <mach-o/dyld.h>
#endif

namespace RTags {

Path encodeSourceFilePath(const Path &dataDir, const Path &project, uint32_t fileId)
{
    String str = dataDir;
    Path p = project;
    encodePath(p);
    str << p << '/';
    if (fileId)
        str << fileId << '/';
    return str;
}


Path findAncestor(Path path, const char *fn, Flags<FindAncestorFlag> flags = Flags<FindAncestorFlag>())
{
    Path ret;
    int slash = path.size();
    const int len = strlen(fn) + 1;
    struct stat st;
    char buf[PATH_MAX + sizeof(dirent) + 1];
    dirent *direntBuf = 0, *entry = 0;
    if (flags & Wildcard)
        direntBuf = reinterpret_cast<struct dirent *>(malloc(sizeof(buf)));

    memcpy(buf, path.constData(), path.size() + 1);
    while ((slash = path.lastIndexOf('/', slash - 1)) > 0) { // We don't want to search in /
        if (!(flags & Wildcard)) {
            memcpy(buf + slash + 1, fn, len);
            if (!stat(buf, &st)) {
                buf[slash + 1] = '\0';
                ret = buf;
                if (flags & Shallow) {
                    break;
                }
            }
        } else {
            buf[slash + 1] = '\0';
            DIR *dir = opendir(buf);
            bool found = false;
            if (dir) {
                while (!readdir_r(dir, direntBuf, &entry) && entry) {
                    const int l = strlen(entry->d_name) + 1;
                    switch (l - 1) {
                    case 1:
                        if (entry->d_name[0] == '.')
                            continue;
                        break;
                    case 2:
                        if (entry->d_name[0] == '.' && entry->d_name[1] == '.')
                            continue;
                        break;
                    }
                    assert(buf[slash] == '/');
                    assert(l + slash + 1 < static_cast<int>(sizeof(buf)));
                    memcpy(buf + slash + 1, entry->d_name, l);
                    if (!fnmatch(fn, buf, 0)) {
                        ret = buf;
                        ret.truncate(slash + 1);
                        found = true;
                        break;
                    }
                }
                closedir(dir);
            }
            if (found && flags & Shallow)
                break;
        }
    }
    if (flags & Wildcard)
        free(direntBuf);

    return ret.ensureTrailingSlash();
}

Map<String, String> rtagsConfig(const Path &path)
{
    Path dir = path.isDir() ? path : path.parentDir();
    Map<String, String> ret;
    if (dir.resolve()) {
        char buf[1024];
        struct stat statBuf;
        while (dir.size() > 1) {
            assert(dir.endsWith('/'));
            snprintf(buf, sizeof(buf), "%s.rtags-config", dir.constData());
            if (!stat(buf, &statBuf) && S_ISREG(statBuf.st_mode)) {
                FILE *f = fopen(buf, "r");
                if (f) {
                    while ((fgets(buf, sizeof(buf), f))) {
                        int len = strlen(buf);
                        while (len > 0 && isspace(buf[len - 1]))
                            --len;
                        if (len) {
                            buf[len] = '\0';
                            String key;
                            char *colon = strchr(buf, ':');
                            char *value = 0;
                            if (colon) {
                                key.assign(buf, colon - buf);
                                value = colon + 1;
                                while (isspace(*value))
                                    ++value;
                            } else {
                                key.assign(buf, len);
                            }
                            if (!key.isEmpty() && !ret.contains(key)) {
                                ret[key] = value;
                            }
                        }
                    }
                    fclose(f);
                }
            }
            dir = dir.parentDir();
        }
    }
    return ret;
}

struct Entry {
    const char *name;
    const Flags<FindAncestorFlag> flags;
};

static inline Path checkEntries(const Entry *entries, const Path &path, const Path &home)
{
    Path best;
    for (int i=0; entries[i].name; ++i) {
        Path p = findAncestor(path, entries[i].name, entries[i].flags);
        if ((p.isEmpty() || p == home) && (entries[i].flags & Wildcard)) {
            const int len = strlen(entries[i].name);
            if (entries[i].name[len - 1] == '*') {
                const String name(entries[i].name, len - 1);
                p = findAncestor(path, name.constData(), entries[i].flags & ~Wildcard);
            }
        }
        if (!p.isEmpty() && p != home && (best.isEmpty() || p.size() < best.size())) {
            best = p;
        }
    }
    return best;
}


Path findProjectRoot(const Path &path, ProjectRootMode mode)
{
    if (!path.isAbsolute())
        error() << "GOT BAD PATH" << path;
    assert(path.isAbsolute());
    const Map<String, String> config = rtagsConfig(path);
    {
        const Path project = config.value("project");
        if (!project.isEmpty() && project.isDir())
            return project.ensureTrailingSlash();
    }

    Path ret;
    static const Path home = Path::home();
    if (mode == SourceRoot) {
        const Entry before[] = {
            { ".git", Flags<FindAncestorFlag>() },
            { ".svn", Flags<FindAncestorFlag>() },
            { ".bzr", Flags<FindAncestorFlag>() },
            { ".tup", Flags<FindAncestorFlag>() },
            { "GTAGS", Flags<FindAncestorFlag>() },
            { "configure", Flags<FindAncestorFlag>() },
            { "CMakeLists.txt", Flags<FindAncestorFlag>() },
            { "*.pro", Wildcard },
            { "scons.1", Flags<FindAncestorFlag>() },
            { "*.scons", Wildcard },
            { "SConstruct", Flags<FindAncestorFlag>() },
            { "autogen.*", Wildcard },
            { "GNUMakefile*", Wildcard },
            { "INSTALL*", Wildcard },
            { "README*", Wildcard },
            { "compile_commands.json", Flags<FindAncestorFlag>() },
            { 0, Flags<FindAncestorFlag>() }
        };
        {
            const Path e = checkEntries(before, path, home);
            if (!e.isEmpty() && (e.size() < ret.size() || ret.isEmpty()))
                ret = e;
        }
    }
    if (!ret.isEmpty())
        return ret;
    {
        const Path configStatus = findAncestor(path, "config.status");
        if (!configStatus.isEmpty()) {
            if (mode == BuildRoot)
                return configStatus;
            FILE *f = fopen((configStatus + "config.status").constData(), "r");
            Path ret;
            if (f) {
                char line[1024];
                enum { MaxLines = 10 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1)
                        break;
                    char *configure = strstr(line, "/configure ");
                    if (configure) {
                        char *end = configure + 10;
                        while (--configure >= line) {
                            Path conf(configure, end - configure);
                            if (!conf.isAbsolute())
                                conf.resolve();
                            if (conf.isFile()) {
                                ret = conf.parentDir();
                                if (ret == home)
                                    ret.clear();
                                break;
                            }
                        }
                    }
                    if (!ret.isEmpty())
                        break;
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    {
        const Path cmakeCache = findAncestor(path, "CMakeCache.txt");
        if (!cmakeCache.isEmpty()) {
            if (mode == BuildRoot)
                return cmakeCache;
            FILE *f = fopen((cmakeCache + "Makefile").constData(), "r");
            bool makefile = true;
            if (!f) {
                f = fopen((cmakeCache + "build.ninja").constData(), "r");
                makefile = false;
            }
            if (f) {
                Path ret;
                char line[1024];
                enum { MaxLines = 256 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1) {
                        break;
                    }

                    if (makefile) {
                        if (!strncmp(line, "CMAKE_SOURCE_DIR", 16)) {
                            char *dir = line + 16;
                            while (*dir && (*dir == ' ' || *dir == '='))
                                ++dir;
                            if (dir != home) {
                                ret = dir;
                                ret += '/';
                                if (!Path(ret + "CMakeLists.txt").isFile())
                                    ret.clear();
                            }
                            break;
                        }
                    } else if (!strncmp(line, "# Write statements declared in CMakeLists.txt:", 46)) {
                        r = Rct::readLine(f, line, sizeof(line));
                        ++i;
                        if (r != -1) {
                            ret = line + 2;
                            if (ret.exists()) {
                                ret = ret.parentDir();
                            } else {
                                ret.clear();
                                break;
                            }
                        }
                    }
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    const Entry after[] = {
        { "build.ninja", Flags<FindAncestorFlag>() },
        { "Makefile*", Wildcard },
        { 0, Flags<FindAncestorFlag>() }
    };

    {
        const Path e = checkEntries(after, path, home);
        if (!e.isEmpty() && (e.size() < ret.size() || ret.isEmpty()))
            ret = e;
    }

    if (!ret.isEmpty())
        return ret;

    if (mode == BuildRoot)
        return findProjectRoot(path, SourceRoot);

    return Path();
}

void initMessages()
{
    Message::registerMessage<IndexMessage>();
    Message::registerMessage<IndexDataMessage>();
    Message::registerMessage<LogOutputMessage>();
    Message::registerMessage<QueryMessage>();
    Message::registerMessage<VisitFileMessage>();
    Message::registerMessage<VisitFileResponseMessage>();
}
}
