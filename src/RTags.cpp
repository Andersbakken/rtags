/* This file is part of RTags.

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
#include "CursorInfo.h"
#include "Server.h"
#include "Str.h"
#include "VisitFileMessage.h"
#include "IndexerMessage.h"
#include <dirent.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <rct/Messages.h>
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

#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#include <cxxabi.h>

static inline char *demangle(const char *str)
{
    if (!str)
        return 0;
    int status;
#ifdef OS_Darwin
    char paren[1024];
    sscanf(str, "%*d %*s %*s %s %*s %*d", paren);
#else
    const char *paren = strchr(str, '(');
    if (!paren) {
        paren = str;
    } else {
        ++paren;
    }
#endif
    size_t l;
    if (const char *plus = strchr(paren, '+')) {
        l = plus - paren;
    } else {
        l = strlen(paren);
    }

    char buf[1024];
    size_t len = sizeof(buf);
    if (l >= len)
        return 0;
    memcpy(buf, paren, l + 1);
    buf[l] = '\0';
    char *ret = abi::__cxa_demangle(buf, 0, 0, &status);
    if (status != 0) {
        if (ret)
            free(ret);
#ifdef OS_Darwin
        return strdup(paren);
#else
        return 0;
#endif
    }
    return ret;
}

String backtrace(int maxFrames)
{
    enum { SIZE = 1024 };
    void *stack[SIZE];

    int frameCount = backtrace(stack, sizeof(stack) / sizeof(void*));
    if (frameCount <= 0)
        return String("Couldn't get stack trace");
    String ret;
    char **symbols = backtrace_symbols(stack, frameCount);
    if (symbols) {
        char frame[1024];
        for (int i=1; i<frameCount && (maxFrames < 0 || i - 1 < maxFrames); ++i) {
            char *demangled = demangle(symbols[i]);
            snprintf(frame, sizeof(frame), "%d/%d %s\n", i, frameCount - 1, demangled ? demangled : symbols[i]);
            ret += frame;
            if (demangled)
                free(demangled);
        }
        free(symbols);
    }
    return ret;
}
#else
String backtrace(int)
{
    return String();
}
#endif

void dirtySymbolNames(SymbolNameMap &map, const Set<uint32_t> &dirty)
{
    SymbolNameMap::iterator it = map.begin();
    while (it != map.end()) {
        Set<Location> &locations = it->second;
        Set<Location>::iterator i = locations.begin();
        while (i != locations.end()) {
            if (dirty.contains(i->fileId())) {
                locations.erase(i++);
            } else {
                ++i;
            }
        }
        if (locations.isEmpty()) {
            map.erase(it++);
        } else {
            ++it;
        }
    }
}

void dirtySymbols(SymbolMap &map, const Set<uint32_t> &dirty)
{
    SymbolMap::iterator it = map.begin();
    while (it != map.end()) {
        if (dirty.contains(it->first.fileId())) {
            map.erase(it++);
        } else {
            CursorInfo &cursorInfo = it->second;
            cursorInfo.dirty(dirty);
            ++it;
        }
    }
}
void dirtyUsr(UsrMap &map, const Set<uint32_t> &dirty)
{
    UsrMap::iterator it = map.begin();
    while (it != map.end()) {
        Set<Location> &locations = it->second;
        Set<Location>::iterator i = locations.begin();
        while (i != locations.end()) {
            if (dirty.contains(i->fileId())) {
                locations.erase(i++);
            } else {
                ++i;
            }
        }
        if (locations.isEmpty()) {
            map.erase(it++);
        } else {
            ++it;
        }
    }
}
/* Same behavior as rtags-default-current-project() */

enum FindAncestorFlag {
    Shallow = 0x1,
    Wildcard = 0x2
};
static inline Path findAncestor(Path path, const char *fn, unsigned flags)
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
            }
            closedir(dir);
            if (found && flags & Shallow)
                break;
        }
    }
    if (flags & Wildcard)
        free(direntBuf);

    return ret.ensureTrailingSlash();
}

struct Entry {
    const char *name;
    const unsigned flags;
};

static inline Path checkEntry(const Entry *entries, const Path &path, const Path &home)
{
    for (int i=0; entries[i].name; ++i) {
        Path p = findAncestor(path, entries[i].name, entries[i].flags);
        if ((p.isEmpty() || p == home) && (entries[i].flags & Wildcard)) {
            const int len = strlen(entries[i].name);
            if (entries[i].name[len - 1] == '*') {
                const String name(entries[i].name, len - 1);
                p = findAncestor(path, name.constData(), entries[i].flags & ~Wildcard);
            }
        }
        if (!p.isEmpty() && p != home) {
            return p;
        }
    }
    return Path();
}


Path findProjectRoot(const Path &path)
{
    assert(path.isAbsolute());
    const Path config = findAncestor(path, ".rtags-config", Shallow);
    if (config.isDir()) {
        const List<String> conf = Path(config + ".rtags-config").readAll().split('\n');
        for (List<String>::const_iterator it = conf.begin(); it != conf.end(); ++it) {
            const char *ch = it->constData();
            while (*ch && isspace(*ch))
                ++ch;
            if (*ch && !strncmp("project: ", ch, 9)) {
                ch += 9;
                while (*ch && isspace(*ch))
                    ++ch;
                const Path p = ch;
                if (p.isDir()) {
                    return p.ensureTrailingSlash();
                } else {
                    error("Invalid project root %s", p.constData());
                }
            }
        }
    }

    static const Path home = Path::home();
    const Entry before[] = {
        { "GTAGS", 0 },
        { "CMakeLists.txt", 0 },
        { "configure", 0 },
        { ".git", 0 },
        { ".svn", 0 },
        { "*.pro", Wildcard },
        { "scons.1", 0 },
        { "*.scons", Wildcard },
        { "SConstruct", 0 },
        { "autogen.*", Wildcard },
        { "GNUMakefile*", Wildcard },
        { "INSTALL*", Wildcard },
        { "README*", Wildcard },
        { 0, 0 }
    };
    {
        const Path ret = checkEntry(before, path, home);
        if (!ret.isEmpty())
            return ret;
    }
    {
        const Path configStatus = findAncestor(path, "config.status", 0);
        if (!configStatus.isEmpty()) {
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
        const Path cmakeCache = findAncestor(path, "CMakeCache.txt", 0);
        if (!cmakeCache.isEmpty()) {
            FILE *f = fopen((cmakeCache + "Makefile").constData(), "r");
            if (f) {
                Path ret;
                char line[1024];
                enum { MaxLines = 256 };
                for (int i=0; i<MaxLines; ++i) {
                    int r = Rct::readLine(f, line, sizeof(line));
                    if (r == -1) {
                        break;
                    }
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
                }
                fclose(f);
                if (!ret.isEmpty())
                    return ret;
            }
        }
    }
    const Entry after[] = {
        { "Makefile*", Wildcard },
        { 0, 0 }
    };

    {
        const Path ret = checkEntry(after, path, home);
        if (!ret.isEmpty())
            return ret;
    }

    return Path();
}

String filterPreprocessor(const Path &path)
{
    String ret;
    FILE *f = fopen(path.constData(), "r");
    if (f) {
        char line[1026];
        int r;
        while ((r = Rct::readLine(f, line, sizeof(line) - 1)) != -1) {
            int start = 0;
            while (start < r && isspace(line[start]))
                ++start;
            if (start == r || line[start] != '#')
                continue;
            line[r] = '\n';
            ret.append(line, r + 1);

            int end = r - 1;
            while (end >= start && isspace(line[end]))
                --end;
            while ((r = Rct::readLine(f, line, sizeof(line) - 1)) != -1) {
                line[r] = '\n';
                ret.append(line, r + 1);
                end = r - 1;
                while (end >= 0 && isspace(line[end]))
                    --end;
                if (end < 0 || line[end] != '\\') {
                    break;
                }
            }
        }

        fclose(f);
    }

    return ret;
}

void initMessages()
{
#ifndef GRTAGS
    Messages::registerMessage<QueryMessage>();
    Messages::registerMessage<CompletionMessage>();
    Messages::registerMessage<CompileMessage>();
    Messages::registerMessage<CreateOutputMessage>();
    Messages::registerMessage<VisitFileMessage>();
    Messages::registerMessage<IndexerMessage>();
#endif
}
}

#ifdef RTAGS_DEBUG_MUTEX
void Mutex::lock()
{
    Timer timer;
    while (!tryLock()) {
        usleep(10000);
        if (timer.elapsed() >= 10000) {
            error("Couldn't acquire lock in 10 seconds\n%s", RTags::backtrace().constData());
            timer.restart();
        }
    }
}
#endif
