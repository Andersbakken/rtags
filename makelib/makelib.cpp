#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

typedef int (*XStat64)(int, const char*, struct stat64*);
typedef int (*XStat)(int, const char*, struct stat*);
typedef int (*Stat64)(const char*, struct stat64*);
typedef int (*Stat)(const char*, struct stat*);
template <typename T>
int sharedStat(int ret, const char *filename, T *stat_buf)
{
    if (!ret && S_ISREG(stat_buf->st_mode)) {
        const int len = strlen(filename);
        struct {
            const char* data;
            int len;
        } static const statData[] = {
            { ".o", 2 },
            { ".lo", 3 },
            { ".gch/c++", 8 },
            { ".gch/c", 6 },
            { 0, 0 }
        };
        for (int i=0; statData[i].data; ++i) {
            if (len >= statData[i].len && !strncmp(filename + len - statData[i].len, statData[i].data, statData[i].len))  {
                stat_buf->st_mtime = 1;
                break;
            }
        }
    }
    return ret;
}

int __xstat64(int ver, const char *filename, struct stat64 *stat_buf)
{
    static XStat64 realStat = 0;
    if (!realStat)
        realStat = reinterpret_cast<XStat64>(dlsym(RTLD_NEXT, "__xstat64"));
    return sharedStat(realStat(ver, filename, stat_buf), filename, stat_buf);
}
int __xstat(int ver, const char *filename, struct stat *stat_buf)
{
    static XStat realStat = 0;
    if (!realStat)
        realStat = reinterpret_cast<XStat>(dlsym(RTLD_NEXT, "__xstat"));
    return sharedStat(realStat(ver, filename, stat_buf), filename, stat_buf);
}
int stat(const char *filename, struct stat *stat_buf)
{
    static Stat realStat = 0;
    if (!realStat)
        realStat = reinterpret_cast<Stat>(dlsym(RTLD_NEXT, "stat"));
    return sharedStat(realStat(filename, stat_buf), filename, stat_buf);
}

int stat64(const char *filename, struct stat64 *stat_buf)
{
    static Stat64 realStat = 0;
    if (!realStat)
        realStat = reinterpret_cast<Stat64>(dlsym(RTLD_NEXT, "stat64"));
    return sharedStat(realStat(filename, stat_buf), filename, stat_buf);
}

