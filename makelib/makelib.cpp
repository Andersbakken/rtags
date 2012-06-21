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
typedef int (*Execve)(const char *, char *const [], char *const []);

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
            { ".gch", 4 },
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
        static const bool log = getenv("LOG_MAKELIB");
        if (log) {
            FILE *f = fopen("/tmp/log", "a");
            fprintf(f, "%s => %ld\n", filename, stat_buf->st_mtime);
            fclose(f);
        }
    }
    return ret;
}

#ifdef __GLIBC__
int __xstat64(int ver, const char *filename, struct stat64 *stat_buf)
{
    static XStat64 realStat = 0;
    if (!realStat)
        realStat = reinterpret_cast<XStat64>(dlsym(RTLD_NEXT, "__xstat64"));
    return sharedStat(realStat(ver, filename, stat_buf), filename, stat_buf);
}
#endif

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

#ifdef __GLIBC__
int stat64(const char *filename, struct stat64 *stat_buf)
{
    static Stat64 realStat = 0;
    if (!realStat)
        realStat = reinterpret_cast<Stat64>(dlsym(RTLD_NEXT, "stat64"));
    return sharedStat(realStat(filename, stat_buf), filename, stat_buf);
}
#endif

int execv(const char *filename, char *const argv[])
{
    FILE *f = fopen("/tmp/fuck", "w");
    fprintf(f, "%s\n", filename);
    fclose(f);
    exit(0);
}

int execve(const char *filename, char *const argv[], char *const envp[])
{
    static const bool log = getenv("LOG_MAKELIB");
    if (log) {
        FILE *f = fopen("/tmp/log", "a");
        fprintf(f, "execve %s", filename);
        for (int i=0; argv[i]; ++i) {
            fprintf(f, " %s", argv[i]);
        }
        fprintf(f, " =>");

        for (int i=0; envp[i]; ++i) {
            fprintf(f, " %s", envp[i]);
        }
        fprintf(f, "\n");
        fclose(f);
    }
    static Execve realExecve = 0;
    if (!realExecve)
        realExecve = reinterpret_cast<Execve>(dlsym(RTLD_NEXT, "execve"));

    struct {
        const char *data;
        int len;
    } static const execveData[] = {
        { "gcc", 3 },
        { "cxx", 3 },
        { "cc", 2 },
        { "cc1", 3 },
        { "g++", 3 },
        { 0, 0 }
    };
    const int len = strlen(filename);
    for (int i=0; execveData[i].data; ++i) {
        if (len >= execveData[i].len && !strncmp(filename + len - execveData[i].len, execveData[i].data, execveData[i].len))  {
            struct stat st;
            if (stat(filename, &st))
                break;

            fprintf(stdout, "yo yo yo %s", filename);
            for (int j=0; argv[j]; ++j) {
                fprintf(stdout, " %s", argv[j]);
            }
            fprintf(stdout, "\n");
            exit(0);
        }
    }
    return realExecve(filename, argv, envp);
}
