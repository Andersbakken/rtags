#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

struct StatData
{
    const char* data;
    int len;
};
static StatData statData[] = {
    { ".o",       2 },
    { ".lo",      3 },
    { ".gch/c++", 8 },
    { ".gch/c",   6 },
    { 0,          0 }
};

#if defined OS_LINUX || defined OS_MACOSX
typedef int (*RealStat)(int, const char*, struct stat64*);
#elif OS_FREEBSD
typedef int (*RealStat)(const char*, struct stat*);
#endif
static RealStat realStat = 0;

#if defined OS_LINUX || defined OS_MACOSX
int __xstat64 (int ver, const char *filename, struct stat64 *stat_buf)
#elif OS_FREEBSD
int stat (const char *filename, struct stat *stat_buf)
#endif
{
    if (!realStat) {
#if defined OS_LINUX || defined OS_MACOSX
        realStat = reinterpret_cast<RealStat>(dlsym(RTLD_NEXT, "__xstat64"));
#elif OS_FREEBSD
        realStat = reinterpret_cast<RealStat>(dlsym(RTLD_NEXT, "stat"));
#endif
    }
#if defined OS_LINUX || defined OS_MACOSX
    int ret = realStat(ver, filename, stat_buf);
#elif OS_FREEBSD
    int ret = realStat(filename, stat_buf);
#endif
    if (!ret && S_ISREG(stat_buf->st_mode)) {
        const int len = strlen(filename);
        bool changed = false;
        for (StatData* current = statData; current->data; ++current) {
            const int& currentLen = current->len;
            if (len >= currentLen && !strncmp(filename + len - currentLen, current->data, currentLen))  {
                stat_buf->st_mtime = 1;
                changed = true;
                break;
            }
        }
        static bool debug = getenv("DEBUG_STAT");
        if (debug) {
            FILE* logfile = fopen("/tmp/makelib.log", "a");
            if (logfile) {
                fprintf(logfile, "stated [%s]%s\n", filename, changed ? " changed" : "");
                fclose(logfile);
            }
        }
    }
    return ret;
}
