#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <string>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <limits.h>
#include <cstdarg>

typedef int (*XStat64)(int, const char*, struct stat64*);
typedef int (*Stat64)(const char*, struct stat64*);
typedef int (*XStat)(int, const char*, struct stat*);
typedef int (*Stat)(const char*, struct stat*);
typedef int (*Execv)(const char *, char *const []);
typedef int (*Execve)(const char *, char *const [], char *const []);

class Log
{
public:
    ~Log()
    {
        if (!out.empty()) {
            static bool first = false; //true;
            FILE *f;
            if (first) {
                f = fopen("/tmp/makelib.log", "w");
                first = false;
            } else {
                f = fopen("/tmp/makelib.log", "a");
            }
            fwrite(out.data(), sizeof(char), out.size(), f);
            fclose(f);
        }
    }
    void log(const char *format, ...)
    {
        va_list args;
        va_start(args, format);
        char buf[1024];
        vsnprintf(buf, sizeof(buf), format, args);
        va_end(args);
        out += buf;
    }
    std::string out;
};

#ifdef __GLIBC__
static XStat64 realXStat64()
{
    static XStat64 realXStat64 = reinterpret_cast<XStat64>(dlsym(RTLD_NEXT, "__xstat64"));
    return realXStat64;
}

static Stat64 realStat64()
{
    static Stat64 realStat64 = reinterpret_cast<Stat64>(dlsym(RTLD_NEXT, "stat64"));
    return realStat64;
}
#endif

static Stat realStat()
{
    static Stat realStat = reinterpret_cast<Stat>(dlsym(RTLD_NEXT, "stat"));
    return realStat;
}

static XStat realXStat()
{
    static XStat realXStat = reinterpret_cast<XStat>(dlsym(RTLD_NEXT, "__xstat"));
    return realXStat;
}

template <typename T>
static int sharedStat(int ret, const char *filename, T *stat_buf)
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
        bool found = false;
        for (int i=0; statData[i].data; ++i) {
            if (len >= statData[i].len && !strncmp(filename + len - statData[i].len, statData[i].data, statData[i].len))  {
                found = true;
                stat_buf->st_mtime = 1;
                break;
            }
        }
        Log log;
        log.log("stat %s%s\n", filename, found ? " => faked dirtyness" : "");
    }
    return ret;
}

#ifdef __GLIBC__
int __xstat64(int ver, const char *filename, struct stat64 *stat_buf)
{
    return sharedStat(realXStat64()(ver, filename, stat_buf), filename, stat_buf);
}
#endif

int __xstat(int ver, const char *filename, struct stat *stat_buf)
{
    return sharedStat(realXStat()(ver, filename, stat_buf), filename, stat_buf);
}

int stat(const char *filename, struct stat *stat_buf)
{
    return sharedStat(realStat()(filename, stat_buf), filename, stat_buf);
}

#ifdef __GLIBC__
int stat64(const char *filename, struct stat64 *stat_buf)
{
    return sharedStat(realStat64()(filename, stat_buf), filename, stat_buf);
}
#endif

static bool eatExec(const char *filename, const char *function, char *const argv[])
{
    Log log;
    log.log("%s %s", function, filename);
    for (int i=0; argv[i]; ++i) {
        log.log(" %s", argv[i]);
    }
    log.log("\n");
    struct {
        const char *data;
        int len;
    } static const execvData[] = {
        { "sh", 2 },
        { "/sed", 4 },
        { "/awk", 4 },
        { "/perl", 5 },
        { "/make", 5 },
        { "/gmake", 6 },
        { 0, 0 }
    };
    const int len = strlen(filename);
    for (int i=0; execvData[i].data; ++i) {
        if (len >= execvData[i].len && !strncmp(filename + len - execvData[i].len, execvData[i].data, execvData[i].len))  {
            log.log("Didn't eat %s", filename);
            for (int j=0; argv[j]; ++j)
                log.log(" %s", argv[j]);
            log.log("\n");
            return false;
        }
    }
    log.log("Ate %s", filename);
    for (int j=0; argv[j]; ++j)
        log.log(" %s", argv[j]);
    log.log("\n");
    return true;
}

int execve(const char *filename, char *const argv[], char *const envp[])
{
    if (eatExec(filename, __FUNCTION__, argv))
        _exit(0);
    static Execve realExecve = reinterpret_cast<Execve>(dlsym(RTLD_NEXT, "execve"));
    return realExecve(filename, argv, envp);
}

int execv(const char *filename, char *const argv[])
{
    if (eatExec(filename, __FUNCTION__, argv))
        _exit(0);
    static Execv realExecv = reinterpret_cast<Execv>(dlsym(RTLD_NEXT, "execv"));
    return realExecv(filename, argv);
}

int execvp(const char *filename, char *const argv[])
{
    if (eatExec(filename, __FUNCTION__, argv))
        _exit(0);
    static Execv realExecvp = reinterpret_cast<Execv>(dlsym(RTLD_NEXT, "execvp"));
    return realExecvp(filename, argv);
}
int execvpe(const char *filename, char *const argv[], char *const envp[])
{
    if (eatExec(filename, __FUNCTION__, argv))
        _exit(0);
    static Execve realExecvpe = reinterpret_cast<Execve>(dlsym(RTLD_NEXT, "execvpe"));
    return realExecvpe(filename, argv, envp);
}
