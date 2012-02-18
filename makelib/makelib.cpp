#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

typedef int (*RealStat)(int, const char*, struct stat64*);
RealStat realStat = 0;
int __xstat64 (int ver, const char *filename, struct stat64 *stat_buf)
{
    if (!realStat) {
        realStat = reinterpret_cast<RealStat>(dlsym(RTLD_NEXT, "__xstat64"));
    }
    int ret = realStat(ver, filename, stat_buf);
    if (!ret && S_ISREG(stat_buf->st_mode)) {
        const int len = strlen(filename);
        bool changed = false;
        if (len >= 2 && !strncmp(filename + len - 2, ".o", 2)) {
            stat_buf->st_mtime = 1;
            changed = true;
        } else if (len >= 3 && !strncmp(filename + len - 3, ".lo", 3)) {
            stat_buf->st_mtime = 1;
            changed = true;
        }
        static bool debug = getenv("DEBUG_STAT");
        if (debug)
            fprintf(stderr, "stated [%s]%s\n", filename, changed ? " changed" : "");
    }
    return ret;
}
